;;; quelpa-rcp.el --- quelpa is a tool to compile and install Emacs Lisp packages locally from local or remote source code.

;;; Code:
(use-package quelpa
  :ensure t
  :defer t
  :custom
  (quelpa-update-melpa-p nil "Don't update the MELPA git repo."))

(use-package quelpa-use-package
  :ensure t
  :init
  (setq quelpa-use-package-inhibit-loading-quelpa t)
  :ensure t)

(provide 'quelpa-rcp)
;;; Commentary:
;;
;;; quelpa-rcp.el ends here
