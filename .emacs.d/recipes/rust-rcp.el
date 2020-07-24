;;; rust-rcp.el --- Rust support

;;; Commentary:
;; 

;;; Code:

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :custom
  (rust-format-on-save t)
)

(use-package flycheck-rust
  :ensure t
  :after flycheck
  :hook
  (flycheck-mode . flycheck-rust-setup)
)

(provide 'rust-rcp)

;;; rust-rcp.el ends here
