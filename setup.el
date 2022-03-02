(require 'lsp)

;; (setq lsp-log-io t)
;; use (lsp-workspace-show-log)
;; (setq lsp-disabled-clients '(sqls))
;; (setq exec-path (append exec-path '("/home/al/src/rsls")))
;; (add-to-list 'lsp-language-id-configuration '(sql-mode . "sql"))

;; (lsp--client-disabled-p 'sql-mode 'sqls)

(defcustom lsp-rsls-executable "rsls"
  "rsls executable path."
  :group 'lsp-rsls
  :risky t
  :type 'file)

(defcustom-lsp lsp-rsls-databases []
               "sqlite database filenames/URIs."
               :group 'lsp-rsls
               :lsp-path "rsls.databases"
               :type '(repeat string))

;; TODO: formatting options

;; (lsp-register-custom-settings '(("rsls.databases" lsp-rsls-databases)))

;; (remhash 'rsls lsp-clients)

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () lsp-rsls-executable))
  :major-modes '(sql-mode)
  :priority -1
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration
                       (lsp-configuration-section "rsls"))))
  :activation-fn (lsp-activate-on "sql")
  :server-id 'rsls
  :synchronize-sections '("rsls")))

