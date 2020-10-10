;;; rust-rcp.el --- Rust support

;;; Code:
(use-package rust-mode
  :ensure t
  :defer t
  :mode "\\.rs\\'"
  :custom
  (rust-indent-offset  2)
  (rust-format-on-save t)
)

(use-package flycheck-rust
  :ensure t
  :defer t
  :after flycheck
  :hook
  (flycheck-mode . flycheck-rust-setup)
)

(provide 'rust-rcp)

;;; Commentary:
;;
;;; rust-rcp.el ends here
