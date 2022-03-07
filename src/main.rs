use fallible_iterator::FallibleIterator;
use itertools::Itertools;
use lsp_server::{Connection, Message, Notification, Request, RequestId, Response};
use lsp_types::{
    notification::{
        DidChangeConfiguration, DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument,
        Notification as _, PublishDiagnostics,
    },
    Diagnostic, DiagnosticSeverity, GotoDefinitionResponse, Hover, HoverContents,
    HoverProviderCapability, InitializeParams, MarkupContent, MarkupKind, Position,
    PublishDiagnosticsParams, Range, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind, TextDocumentSyncOptions, TextEdit, Url,
};
use ropey::Rope;
use serde::Deserialize;
use sqlformat::{FormatOptions, QueryParams};
use sqlite3_parser::{
    ast::{As, Cmd, Expr, Id, Name, OneSelect, ResultColumn, SelectTable, Stmt},
    lexer::{sql::Error as ParseError, sql::Parser, InputStream},
};
use std::{collections::HashMap, error::Error};

#[derive(Deserialize, Debug)]
struct Settings {
    databases: Vec<String>,
}

struct Doc {
    version: i32,
    rope: Rope,
    ast: Option<Result<Vec<(Cmd, Range, Option<String>)>, ParseError>>,
}

impl Doc {
    fn parse(&mut self) {
        // impl Input?
        let s = self.rope.slice(..).to_string();
        let input = InputStream::new(s.as_bytes());
        let mut parser = Parser::new(input);
        let mut cmds = Vec::new();
        let pos = |p: &Parser<_>| Position::new((p.line() - 1) as u32, p.column() as u32);

        loop {
            // these positions are off because they include white
            // space surrounding statements - line here is 1-based
            let start = pos(&parser);
            match parser.next() {
                Ok(None) => {
                    self.ast = Some(Ok(cmds));
                    return;
                }
                Err(err) => {
                    self.ast = Some(Err(err));
                    return;
                }
                Ok(Some(cmd)) => {
                    cmds.push((cmd, Range::new(start, pos(&parser)), None));
                }
            }
        }
    }

    fn set_hover_info(&mut self, conn: &rusqlite::Connection) {
        if let Some(Ok(cmds)) = &mut self.ast {
            for (cmd, _, ref mut info) in cmds {
                *info = hover_info(conn, &cmd);
            }
        }
    }
}

#[derive(Debug)]
struct ColumnInfo {
    //name: String,
    ty: Option<String>,
    notnull: bool,
    default: Option<String>,
    pk: bool,
}

impl std::fmt::Display for ColumnInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ty: {:?} notnull: {} default: {:?} pk: {}",
            self.ty, self.notnull, self.default, self.pk
        )
    }
}

// find column info for columns used by cmd
#[allow(unstable_name_collisions)]
fn hover_info(conn: &rusqlite::Connection, cmd: &Cmd) -> Option<String> {
    let mut pragma = conn.prepare("select * from pragma_table_info(?);").unwrap();

    match cmd {
        // TODO: insert/update
        Cmd::Explain(stmt) | Cmd::ExplainQueryPlan(stmt) | Cmd::Stmt(stmt) => match stmt {
            Stmt::Insert {
                tbl_name: _,
                columns: _,
                ..
            } => None,
            Stmt::Select(select) => match &select.body.select {
                OneSelect::Select {
                    columns,
                    from: Some(from),
                    ..
                } => {
                    let name_alias = |t: &SelectTable| match t {
                        SelectTable::Table(name, alias, _) => Some((
                            name.name.0.clone(),
                            alias.as_ref().and_then(|a| match a {
                                As::As(alias) => Some(alias.0.clone()),
                                _ => None,
                            }),
                        )),
                        _ => None,
                    };

                    let table = from.select.iter().filter_map(|t| name_alias(t.as_ref()));

                    let joins = from
                        .joins
                        .iter()
                        .flatten()
                        .filter_map(|t| name_alias(&t.table));

                    let tables: Vec<_> = table
                        .chain(joins)
                        .map(|(name, alias)| {
                            let column_info: HashMap<String, ColumnInfo> = pragma
                                .query_map(&[&name], |row| {
                                    let name: String = row.get(1)?;
                                    Ok((
                                        name,
                                        ColumnInfo {
                                            // this does not do None (ever)?
                                            ty: row.get(2)?,
                                            notnull: row.get(3)?,
                                            default: row.get(4)?,
                                            pk: row.get(5)?,
                                        },
                                    ))
                                })
                                .unwrap()
                                // this just discards errors?
                                //.flatten()
                                .filter_map(|res| match res {
                                    Ok(pair) => Some(pair),
                                    Err(err) => {
                                        eprintln!("error in query_map: {}", err);
                                        None
                                    }
                                })
                                .collect();

                            (name, alias, column_info)
                        })
                        .collect();

                    //eprintln!("tables {:?}", tables);

                    // TODO: this is not FromIterator for Option<_>
                    match columns
                        .iter()
                        .filter_map(|c| match c {
                            ResultColumn::Expr(expr, _) => match expr {
                                Expr::Id(Id(col)) => tables
                                    .first()
                                    .and_then(|(_, _, cols)| cols.get(col))
                                    .map(|info| (col, info)),
                                Expr::Qualified(Name(table), Name(col)) => todo!(),
                                _ => None,
                            },
                            _ => None,
                        })
                        .map(|(col, info)| format!("{}: [{}]", col, info))
                        .intersperse(", ".to_string())
                        .collect::<String>()
                        .as_str()
                    {
                        "" => None,
                        a => Some(a.to_string()),
                    }
                }
                _ => None,
            },
            Stmt::Update { tbl_name, sets, .. } => None,
            _ => None,
        },
    }
}

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    // Note that  we must have our logging only write out to stderr.
    eprintln!("starting LSP server");

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = Connection::stdio();

    // Run the server and wait for the two threads to end (typically
    // by trigger LSP Exit event).
    let mut caps = ServerCapabilities::default();
    caps.text_document_sync = Some(TextDocumentSyncCapability::from(TextDocumentSyncOptions {
        open_close: Some(true),
        change: Some(TextDocumentSyncKind::FULL),
        will_save: None,
        will_save_wait_until: None,
        save: None,
    }));
    caps.document_formatting_provider = Some(lsp_types::OneOf::Left(true));
    caps.hover_provider = Some(HoverProviderCapability::Simple(true));
    let server_capabilities = serde_json::to_value(&caps).unwrap();
    let initialization_params = connection.initialize(server_capabilities)?;
    main_loop(connection, initialization_params)?;
    io_threads.join()?;

    // Shut down gracefully.
    eprintln!("shutting down server");
    Ok(())
}

fn publish_diagnostics(conn: &rusqlite::Connection, rope: &Rope, uri: &Url) -> Message {
    let sql = rope.slice(..).to_string();
    let d = conn.prepare(&sql).err().map(|err| {
        let offset = unsafe { rusqlite::ffi::sqlite3_error_offset(conn.handle()) };
        eprintln!("offset: {}", offset);
        let range = if offset >= 0 {
            let offset = offset as usize;
            range(rope, offset..rope.len_chars())
        } else {
            Range::default()
        };
        Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            message: err.to_string(),
            ..Default::default()
        }
    });

    Message::Notification(Notification::new(
        PublishDiagnostics::METHOD.into(),
        PublishDiagnosticsParams {
            uri: uri.clone(),
            diagnostics: d.into_iter().collect(),
            version: None,
        },
    ))
}

fn main_loop(
    connection: Connection,
    params: serde_json::Value,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    let mut docs: HashMap<lsp_types::Url, Doc> = HashMap::new();
    let mut conns: Vec<rusqlite::Connection> = Vec::new();

    eprintln!("starting example main loop");

    for msg in &connection.receiver {
        eprintln!("got msg: {:?}", msg);
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                eprintln!("got request: {:?}", req);
                let req = match cast::<lsp_types::request::GotoDefinition>(req) {
                    Ok((id, params)) => {
                        eprintln!("got gotoDefinition request #{}: {:?}", id, params);
                        let result = Some(GotoDefinitionResponse::Array(Vec::new()));
                        let result = serde_json::to_value(&result).unwrap();
                        let resp = Response {
                            id,
                            result: Some(result),
                            error: None,
                        };
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(req) => req,
                };

                let req = match cast::<lsp_types::request::Formatting>(req) {
                    Ok((id, params)) => {
                        if let Some(doc) = docs.get(&params.text_document.uri) {
                            let query = doc.rope.slice(..).to_string();
                            let new_text = sqlformat::format(
                                &query,
                                &QueryParams::None,
                                FormatOptions::default(),
                            );
                            let result = TextEdit {
                                range: range(&doc.rope, 0..doc.rope.len_chars()),
                                new_text,
                            };
                            let result = Some(vec![result]);
                            let result = serde_json::to_value(&result).unwrap();
                            let resp = Response {
                                id,
                                result: Some(result),
                                error: None,
                            };
                            connection.sender.send(Message::Response(resp))?;
                        }
                        continue;
                    }
                    Err(req) => req,
                };

                let _req = match cast::<lsp_types::request::HoverRequest>(req) {
                    Ok((id, params)) => {
                        let position = params.text_document_position_params.position;
                        let uri = params.text_document_position_params.text_document.uri;

                        if let Some(doc) = docs.get(&uri) {
                            if let Some(Ok(cmds)) = &doc.ast {
                                if let Some((_, _, hover_info)) =
                                    cmds.iter().find(|(_, range, _)| in_range(*range, position))
                                {
                                    //eprintln!("hover_info: {:?}", hover_info);
                                    if let Some(hover_info) = hover_info {
                                        connection
                                            .sender
                                            .send(Message::Response(Response::new_ok(
                                                id,
                                                Hover {
                                                    contents: HoverContents::Markup(
                                                        MarkupContent {
                                                            kind: MarkupKind::PlainText,
                                                            value: hover_info.clone(),
                                                        },
                                                    ),
                                                    range: Default::default(),
                                                },
                                            )))
                                            .unwrap();
                                    }
                                }
                            }
                        }
                        continue;
                    }
                    Err(req) => req,
                };

                // ...
            }
            Message::Response(resp) => {
                eprintln!("got response: {:?}", resp);
            }
            Message::Notification(not) => {
                eprintln!("got notification: {:?}", not);
                let not = match castn::<DidOpenTextDocument>(not) {
                    Ok(params) => {
                        let rope = Rope::from_str(&params.text_document.text);

                        let mut doc = Doc {
                            version: params.text_document.version,
                            rope,
                            ast: None,
                        };

                        doc.parse();

                        if let Some(c) = conns.first() {
                            connection
                                .sender
                                .send(publish_diagnostics(c, &doc.rope, &params.text_document.uri))
                                .unwrap();

                            doc.set_hover_info(c);
                        }

                        docs.insert(params.text_document.uri.clone(), doc);

                        continue;
                    }
                    Err(not) => not,
                };

                let not = match castn::<DidChangeTextDocument>(not) {
                    Ok(params) => {
                        match docs.get_mut(&params.text_document.uri) {
                            Some(doc) => {
                                for change in params.content_changes {
                                    let rope = Rope::from_str(&change.text);
                                    // make sure range is None?
                                    doc.rope = rope;
                                }
                                doc.parse();
                                // TODO: use parse error for
                                // diagnostics - also send on open?
                                if let Some(c) = conns.first() {
                                    doc.set_hover_info(c);

                                    connection
                                        .sender
                                        .send(publish_diagnostics(
                                            c,
                                            &doc.rope,
                                            &params.text_document.uri,
                                        ))
                                        .unwrap();
                                }
                            }
                            None => {
                                // error
                            }
                        }
                        continue;
                    }
                    Err(not) => not,
                };

                let not = match castn::<DidCloseTextDocument>(not) {
                    Ok(params) => {
                        docs.remove(&params.text_document.uri);
                        continue;
                    }
                    Err(not) => not,
                };

                let _not = match castn::<DidChangeConfiguration>(not) {
                    Ok(params) => {
                        let settings = params.settings.get("rsls").unwrap();
                        let settings: Settings = serde_json::from_value(settings.clone()).unwrap();
                        // make connections
                        conns.clear();
                        for s in settings.databases.iter() {
                            conns.push(rusqlite::Connection::open(s).unwrap());
                        }

                        continue;
                    }
                    Err(not) => not,
                };
            }
        }
    }
    Ok(())
}

// ignoring utf16 for now
fn position(rope: &Rope, offset: usize) -> Position {
    let line = rope.char_to_line(offset);
    Position::new(line as u32, (offset - rope.line_to_char(line)) as u32)
}

fn range(rope: &Rope, range: std::ops::Range<usize>) -> Range {
    Range::new(position(rope, range.start), position(rope, range.end))
}

fn in_range(r: Range, p: Position) -> bool {
    p >= r.start && p <= r.end
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), Request>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

fn castn<N>(not: Notification) -> Result<N::Params, Notification>
where
    N: lsp_types::notification::Notification,
    N::Params: serde::de::DeserializeOwned,
{
    not.extract(N::METHOD)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let mut doc = Doc {
            version: 1,
            rope: Rope::from_str("select * from foo; select * from baz;"),
            ast: None,
        };
        doc.parse();
        assert_eq!(
            match doc.ast {
                Some(Ok(ref v)) => v.len(),
                _ => 0,
            },
            2,
            "expecting 2 statements: {:?}",
            doc.ast
        );
    }
}
