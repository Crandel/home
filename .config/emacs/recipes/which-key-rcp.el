;;; which-key-rcp.el --- Add help for keymapping

;;; Code:
(use-package which-key
  :ensure t
  :defer 1
  :commands (which-key--show-keymap which-key--hide-popup-ignore-command)
  :custom
  (which-key-show-transient-maps t)
  :config
  (which-key-mode))

(provide 'which-key-rcp)
;;; Commentary:
;;
;;; which-key-rcp.el ends here
