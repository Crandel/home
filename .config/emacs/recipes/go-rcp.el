;;; go-rcp.el --- Golang support

;;; Code:
(eval-when-compile (require 'use-package))
(use-package go-ts-mode
  :ensure t
  :mode "\\.go\\'"
  :preface
  (defun vd/go-lsp-start()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t)
    (lsp-deferred)
    )
  :bind
  (:map go-ts-mode-map
    ("C-c g b" . go-dap-setup)
    ("C-c g h" . go-root-setup)
    ("C-c g t" . dap-breakpoint-toggle)
    ("C-c g a" . treesit-beginning-of-defun)
    ("C-c g e" . treesit-end-of-defun)
    ("C-c g i" . prog-indent-sexp)
    ("RET"     . reindent-then-newline-and-indent)
    ("M-RET"   . newline)
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
