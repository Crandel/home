;;; js-rcp.el --- Javascript, json and yaml modes

;;; Code:
(eval-when-compile (require 'use-package))
(use-package js-ts-mode
  :mode ("\\.js\\'" "\\firefoxjs\\'")
  :bind
  (:map js-ts-mode-map
    ("RET"     . reindent-then-newline-and-indent)
    ("M-RET"   . newline)
   )
)

(use-package tsx-ts-mode
  :mode ("\\.tsx\\'" . tsx-ts-mode)
  :bind
  (:map tsx-ts-mode-map
    ("RET"     . reindent-then-newline-and-indent)
    ("M-RET"   . newline)
   )
)

(use-package typescript-ts-mode
  :mode ("\\.ts\\'" . typescript-ts-mode)
  :bind
  (:map typescript-ts-mode-map
    ("RET"     . reindent-then-newline-and-indent)
    ("M-RET"   . newline)
   )
)

(provide 'js-rcp)
;;; Commentary:
;;
;;; js-rcp.el ends here
