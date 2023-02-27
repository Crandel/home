;;; tree-sitter-rcp.el --- The minor mode tree-sitter-mode provides a buffer-local syntax tree, which is kept up-to-date with changes to the buffer’s text.
;;; Code:
(eval-when-compile (require 'use-package))
(use-package tree-sitter
  :ensure t
  :defer t
  :hook (
         (c++-mode       . tree-sitter-hl-mode)
         (c-mode         . tree-sitter-hl-mode)
         (go-mode        . tree-sitter-hl-mode)
         (java-mode      . tree-sitter-hl-mode)
         (js2-mode       . tree-sitter-hl-mode)
         (json-mode      . tree-sitter-hl-mode)
         (rust-mode      . tree-sitter-hl-mode)
         (python-mode    . tree-sitter-hl-mode)
         (sh-mode        . tree-sitter-hl-mode)
         )

)

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter-mode
)

(provide 'tree-sitter-rcp)

;;; Commentary:
;;
;;; tree-sitter-rcp.el ends here
