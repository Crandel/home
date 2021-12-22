;;; restclient-rcp.el --- This is a tool to manually explore and test HTTP REST

;;; Code:
(eval-when-compile (require 'use-package))
(use-package restclient
  :ensure t
  :defer t
  :mode ("\\.rest\\'" . restclient-mode)
)

(provide 'restclient-rcp)
;;; Commentary:
;;
;;; restclient-rcp.el ends here
