;;; all-the-icons-rcp.el --- A utility package to collect various Icon Fonts and propertize them within Emacs.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :custom
  (all-the-icons-dired-monochrome nil)
  :custom-face
  (all-the-icons-dired-dir-face ((t (:foreground "orange"))))
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t))
)

(provide 'all-the-icons-rcp)
;;; Commentary:
;;
;;; all-the-icons-rcp.el ends here
