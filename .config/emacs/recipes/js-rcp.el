;;; js-rcp.el --- Javascript, json and yaml modes

;;; Code:
(eval-when-compile (require 'use-package))
(use-package js-ts-mode
  :mode ("\\.js\\'" . js-ts-mode)
  :bind
  (:map js-ts-mode-map
    ("RET"     . reindent-then-newline-and-indent)
    ("M-RET"   . newline)
   )
)

(provide 'js-rcp)
;;; Commentary:
;;
;;; js-rcp.el ends here
