;;; csv-mode-rcp.el --- CSV mode

;;; Code:
(eval-when-compile (require 'use-package))
(use-package csv-mode
  :ensure t
  :hook
  (csv-mode . csv-align-mode)
)

(provide 'csv-mode-rcp)
;;; Commentary:
;;
;;; csv-mode-rcp.el ends here
