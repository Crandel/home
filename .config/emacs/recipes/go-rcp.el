;;; go-rcp.el --- Golang support

;;; Code:
(eval-when-compile (require 'use-package))
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :preface
  (defun vd/go-lsp-start()
               (add-hook 'before-save-hook #'lsp-format-buffer t t)
               (add-hook 'before-save-hook #'lsp-organize-imports t t)
               (lsp-deferred)
               )
  :hook
  (go-mode    . vd/go-lsp-start)
  (go-ts-mode . vd/go-lsp-start)
  :bind
  (:map go-mode-map
        ("RET" . newline-and-indent)
        ("M-RET" . newline)
        )
  :config
  (add-to-list 'exec-path "~/.local/bin")
  (setq lsp-go-analyses '(
                          (nilness . t)
                          (shadow . t)
                          (unusedwrite . t)
                          (fieldalignment . t)
                                       )
        lsp-go-codelenses '(
                          (test . t)
                          (tidy . t)
                          (upgrade_dependency . t)
                          (vendor . t)
                          (run_govulncheck . t)
                                       )
        )
)

(use-package go-tag
  :ensure t
)

(use-package godoctor
  :ensure t
)

(provide 'go-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; go-rcp.el ends here
