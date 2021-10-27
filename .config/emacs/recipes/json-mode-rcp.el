;;; json-mode-rcp.el --- Json mode

;;; Code:
(eval-when-compile (require 'use-package))
(use-package json-mode
  :defer t
  :ensure t)

(use-package yaml-mode
  :defer t
  :ensure t)

(provide 'json-mode-rcp)
;;; Commentary:
;;
;;; json-mode-rcp.el ends here
