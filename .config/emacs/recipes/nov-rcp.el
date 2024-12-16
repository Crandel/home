;;; nov-rcp.el --- Major mode for reading EPUBs in Emacs
;;; Code:
(eval-when-compile (require 'use-package))
(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
)

(provide 'nov-rcp)
;;; Commentary:
;;
;;; nov-rcp.el ends here
