;;; flycheck-rcp.el --- On-the-fly syntax checking

;;; Code:
(eval-when-compile (require 'use-package))
(use-package flycheck
  :ensure t
  :defer t
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save idle-change))
  (flycheck-emacs-lisp-load-path       'inherit)
  (flycheck-highlighting-mode          'lines)
  (flycheck-indication-mode            'left-fringe)
  (flycheck-scalastylerc               "~/scalastyle_config.xml")
  (flycheck-checker-error-threshold    2000)
  ;; :hook
  ;; (prog-mode . flycheck-mode)
  ;; (web-mode  . flycheck-mode)
  :bind ("C-c C-n" . flycheck-next-error)
)

(use-package flycheck-golangci-lint
  :ensure t
  :hook
  (go-mode . flycheck-golangci-lint-setup)
)

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
