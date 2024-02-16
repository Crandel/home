;;; rust-rcp.el --- Rust support

;;; Code:
(eval-when-compile (require 'use-package))
(use-package rust-ts-mode
  :defer t
  :mode "\\.rs\\'"
  :custom
  (rust-indent-offset  4)
  (rust-format-on-save t)
)


(provide 'rust-rcp)

;;; Commentary:
;;
;;; rust-rcp.el ends here
