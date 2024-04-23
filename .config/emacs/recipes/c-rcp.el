;;; c-rcp.el --- Javascript, json and yaml modes

;;; Code:
(eval-when-compile (require 'use-package))
(use-package c-ts-mode
  :bind
  (:map c-ts-mode-map
    ("RET"     . reindent-then-newline-and-indent)
    ("M-RET"   . newline)
   )
)

(provide 'c-rcp)
;;; Commentary:
;;
;;; c-rcp.el ends here
