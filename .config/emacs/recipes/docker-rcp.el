;;; docker-rcp.el --- Docker support

;;; Code:
(eval-when-compile (require 'use-package))
(use-package dockerfile-mode
  :ensure t
  :defer t)
(use-package docker-compose-mode
  :ensure t
  :defer t)

(provide 'docker-rcp)
;;; Commentary:
;;
;;; docker-rcp.el ends here
