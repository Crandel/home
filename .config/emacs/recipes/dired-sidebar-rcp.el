;;; dired-sidebar-rcp.el --- Sidebar for Emacs leveraging Dired

;;; Code:
(eval-when-compile (require 'use-package))
(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :custom
  (dired-sidebar-theme                'icons)
  (dired-sidebar-use-term-integration t)
  (dired-sidebar-use-custom-font      t)
  :bind
  ([f7] . dired-sidebar-toggle-sidebar)
)

(use-package all-the-icons-dired
  :ensure t
)

(provide 'dired-sidebar-rcp)
;;; Commentary:
;;
;;; dired-sidebar-rcp.el ends here
