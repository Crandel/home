;;; turbo-log-rcp.el --- Turbo log - fast logging selected line or region

;;; Code:
(eval-when-compile (require 'use-package))
(use-package turbo-log
  :load-path "git"
  :bind (("C-s-l" . turbo-log-print)
         ("C-s-i" . turbo-log-print-immediately)
         ("C-s-h" . turbo-log-comment-all-logs)
         ("C-s-s" . turbo-log-uncomment-all-logs)
         ("C-s-[" . turbo-log-paste-as-logger)
         ("C-s-]" . turbo-log-paste-as-logger-immediately)
         ("C-s-d" . turbo-log-delete-all-logs))
  :config
  (setq turbo-log-msg-format-template "\"🚀: %s\"")
  (setq turbo-log-allow-insert-without-tree-sitter-p t))
(provide 'turbo-log-rcp)

;;; Commentary:
;;
;;; turbo-log-rcp.el ends here
