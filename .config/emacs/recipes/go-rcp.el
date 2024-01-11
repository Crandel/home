;;; go-rcp.el --- Golang support

;;; Code:
(eval-when-compile (require 'use-package))
(use-package go-ts-mode
  :ensure t
  :mode "\\.go\\'"
  :preface
  (defun vd/go-lsp-start()
    (define-key go-ts-mode-map
                ["RET"] 'newline-and-indent)
    (define-key go-ts-mode-map
                ["M-RET"] 'newline)
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t)
    (lsp-deferred)
    )
  :custom
  (go-ts-mode-indent-offset 4)
  :config
  (add-to-list 'exec-path "~/.local/bin")
  (setq lsp-go-analyses '(
                          (nilness . t)
                          (shadow . t)
                          (unusedwrite . t)
                          (fieldalignment . t)
                          (escape . t)
                                       )
        lsp-go-codelenses '(
                          (test . t)
                          (tidy . t)
                          (upgrade_dependency . t)
                          (vendor . t)
                          (gc_details . t)
                          (run_govulncheck . t)
                                       )
        )
  :hook
  (go-ts-mode . vd/go-lsp-start)
)

;; (use-package go-tag
;;   :ensure t
;; )

(use-package godoctor
  :ensure t
)

(provide 'go-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; go-rcp.el ends here
