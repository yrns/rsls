use lsp_server::{Connection, Message, Notification, Request, RequestId, Response};
use lsp_types::{
    notification::{DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument},
    GotoDefinitionResponse, InitializeParams, Position, Range, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions, TextEdit,
};
use ropey::Rope;
use sqlformat::{FormatOptions, QueryParams};
use std::{collections::HashMap, error::Error};

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
    let server_capabilities = serde_json::to_value(&caps).unwrap();
    let initialization_params = connection.initialize(server_capabilities)?;
    main_loop(connection, initialization_params)?;
    io_threads.join()?;

    // Shut down gracefully.
    eprintln!("shutting down server");
    Ok(())
}

fn main_loop(
    connection: Connection,
    params: serde_json::Value,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    let mut docs: HashMap<lsp_types::Url, (i32, Rope)> = HashMap::new();

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

                let _req = match cast::<lsp_types::request::Formatting>(req) {
                    Ok((id, params)) => {
                        if let Some((_v, text)) = docs.get(&params.text_document.uri) {
                            let query = text.slice(..).to_string();
                            let new_text = sqlformat::format(
                                &query,
                                &QueryParams::None,
                                FormatOptions::default(),
                            );
                            let result = TextEdit {
                                range: range(text, 0..text.len_chars()),
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
                        docs.insert(
                            params.text_document.uri,
                            (params.text_document.version, rope),
                        );
                        continue;
                    }
                    Err(not) => not,
                };

                let not = match castn::<DidChangeTextDocument>(not) {
                    Ok(params) => {
                        match docs.get_mut(&params.text_document.uri) {
                            Some((_v, text)) => {
                                for change in params.content_changes {
                                    let rope = Rope::from_str(&change.text);
                                    // make sure range is None?
                                    *text = rope;
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

                let _not = match castn::<DidCloseTextDocument>(not) {
                    Ok(params) => {
                        docs.remove(&params.text_document.uri);
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
