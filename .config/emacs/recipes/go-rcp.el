;;; go-rcp.el --- Golang support

;;; Code:
(eval-when-compile (require 'use-package))
(use-package go-mode
  :ensure t
  :defer t
  :mode "\\.go\\'"
  :hook
  (go-mode . (lambda()
               (add-hook 'before-save-hook #'lsp-format-buffer t t)
               (add-hook 'before-save-hook #'lsp-organize-imports t t)
               (lsp)
               ))
  :bind
  (:map go-mode-map
        ("RET" . newline-and-indent)
        ("M-RET" . newline)
        )
)

;; (use-package go-eldoc
;;   :ensure t
;;   :after go-mode
;;   :hook
;;   (go-mode . go-eldoc-setup)
;; )


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
