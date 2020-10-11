;;; ansible-rcp.el --- Ansible minor mode for yaml mode

;;; Code:
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
