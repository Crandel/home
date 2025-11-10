;;; tree-sitter-rcp.el --- The minor mode tree-sitter-mode provides a buffer-local syntax tree, which is kept up-to-date with changes to the buffer’s text.
;;; Code:
(eval-when-compile (require 'use-package))
(use-package treesit
  :custom
  (treesit-font-lock-level 4)
  ;; (major-mode-remap-alist '((c-mode    . c-ts-mode)
  ;;                           (c++-mode  . c++-ts-mode)
  ;;                           (go-mode   . go-ts-mode)
  ;;                           (html-mode . html-ts-mde)
  ;;                           (bash-mode . bash-ts-mode)
  ;;                           (rust-mode . rust-ts-mode)
  ;;                           ))
)

(provide 'tree-sitter-rcp)

;;; Commentary:
;;
;;; tree-sitter-rcp.el ends here
