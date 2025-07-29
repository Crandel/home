;;; turbo-log-rcp.el --- Turbo log - fast logging selected line or region

;;; Code:
(eval-when-compile (require 'use-package))
(use-package turbo-log
  :vc (:url "https://github.com/Artawower/turbo-log" :branch "master")
  :bind (
  (:map go-ts-mode-map
         ("C-c i l p" . turbo-log-print)
         ("C-c i l i" . turbo-log-print-immediately)
         ("C-c i l a" . turbo-log-comment-all-logs)
         ("C-c i l u" . turbo-log-uncomment-all-logs)
         ("C-c i l l" . turbo-log-paste-as-logger)
         ("C-c i l x" . turbo-log-paste-as-logger-immediately)
         ("C-c i l d" . turbo-log-delete-all-logs)))
  :config
  (setq turbo-log-msg-format-template "\"%s\"")
  (setq turbo-log-allow-insert-without-tree-sitter-p t))
(provide 'turbo-log-rcp)

;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; turbo-log-rcp.el ends here
