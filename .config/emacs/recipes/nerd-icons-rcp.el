;;; nerd-icons-rcp.el --- A utility package to collect various Icon Fonts and propertize them within Emacs.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package nerd-icons
  :ensure t
)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
)

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(provide 'nerd-icons-rcp)
;;; Commentary:
;;
;;; nerd-icons-rcp.el ends here
