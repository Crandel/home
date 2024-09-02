;;; flycheck-rcp.el --- On-the-fly syntax checking

;;; Code:
(eval-when-compile (require 'use-package))
(use-package flycheck
  :ensure t
  :defer t
  :init
  (defvar-local flycheck-local-checkers nil)
  (defun vd/flycheck-checker-get(fn checker property)
    (or (alist-get property (alist-get checker flycheck-local-checkers))
        (funcall fn checker property)))
  (advice-add 'flycheck-checker-get :around 'vd/flycheck-checker-get)
  :custom
  (flycheck-keymap-prefix              (kbd "C-c n"))
  (flycheck-check-syntax-automatically '(mode-enabled save idle-change))
  (flycheck-emacs-lisp-load-path       'inherit)
  (flycheck-highlighting-mode          'lines)
  (flycheck-indication-mode            'left-fringe)
  (flycheck-checker-error-threshold    2000)
  :hook
  (prog-mode . flycheck-mode)
  (web-mode  . flycheck-mode)
)

(use-package flycheck-golangci-lint
  :ensure t
  :init
  (add-hook 'go-ts-mode (progn
                  (flycheck-golangci-lint-setup)
                  (setq flycheck-go-local-checkers '((lsp . ((next-checkers . (golangci-lint))))))
                  )
              )
)

(use-package attrap
  :ensure t
  :bind (("C-c n f" . attrap-attrap)))

(use-package flycheck-rust
  :ensure t
  :defer t
  :hook
  (rust-mode . flycheck-rust-setup)
)

(provide 'flycheck-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; flycheck-rcp.el ends here
