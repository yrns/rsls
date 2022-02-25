(setq lsp-log-io t)
;; use (lsp-workspace-show-log)
(setq lsp-disabled-clients '(sqls))
(setq exec-path (append exec-path '("/home/al/src/rsls")))
;; (add-to-list 'lsp-language-id-configuration '(sql-mode . "sql"))
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "rsls")
                  :activation-fn (lsp-activate-on "sql")
                  :server-id 'rsls))
