;;; undo-rcp.el --- Visual undo tree.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package vundo
  :ensure t
  :custom
  (vundo-compact-display   t)
  (vundo-window-max-height 7)
  :bind
  ("C-c u" . vundo)
)

(provide 'undo-rcp)
;;; Commentary:
;;
;;; undo-rcp.el ends here
