;;; rust-rcp.el --- Rust support

;;; Code:
(eval-when-compile (require 'use-package))
(use-package rust-ts-mode
  :defer t
  :mode "\\.rs\\'"
  :custom
  (rust-indent-offset  4)
  (rust-format-on-save t)
  :bind (:map rust-ts-mode-map
    ("C-c g a" . treesit-beginning-of-defun)
    ("C-c g e" . treesit-end-of-defun)
    ("C-c g i" . prog-indent-sexp)
    ("RET"     . reindent-then-newline-and-indent)
    ("M-RET"   . newline)
   )
)


(provide 'rust-rcp)

;;; Commentary:
;;
;;; rust-rcp.el ends here
