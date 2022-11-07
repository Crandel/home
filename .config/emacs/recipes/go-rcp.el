;;; go-rcp.el --- Golang support

;;; Code:
(eval-when-compile (require 'use-package))
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook
  (go-mode . (lambda()
               (add-hook 'before-save-hook #'lsp-format-buffer t t)
               (add-hook 'before-save-hook #'lsp-organize-imports t t)
               (lsp-deferred)
               ))
  :bind
  (:map go-mode-map
        ("RET" . newline-and-indent)
        ("M-RET" . newline)
        )
  :config
  (add-to-list 'exec-path "~/go/bin")
  (setq lsp-go-analyses '(
                          (nilness . t)
                          (shadow . t)
                          (unusedwrite . t)
                          (fieldalignment . t)
                                       ))
)

(use-package go-tag
  :ensure t
)

(use-package flycheck-golangci-lint
  :ensure t
  :hook
  (go-mode . flycheck-golangci-lint-setup)
)

(provide 'go-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;; go get -u github.com/alecthomas/gometalinter
;; gometalinter --install
;; go get -u github.com/rogpeppe/godef
;; go get -u github.com/nsf/gocode
;;; go-rcp.el ends here
