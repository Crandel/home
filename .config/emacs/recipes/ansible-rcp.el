;;; ansible-rcp.el --- Ansible minor mode for yaml mode

;;; Code:
(eval-when-compile (require 'use-package))
(use-package ansible
  :ensure t
  :defer t
  :hook
  (yaml-mode)
)

(provide 'ansible-rcp)
;;; Commentary:
;;
;;; ansible-rcp.el ends here
