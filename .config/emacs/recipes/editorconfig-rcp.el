;;; editorconfig-rcp.el --- This is an EditorConfig plugin for Emacs.

;;; Code:

(use-package editorconfig
  :ensure t
  :hook
  (python-mode   . editorconfig-mode)
  (yaml-mode     . editorconfig-mode)
  (sh-mode       . editorconfig-mode)
  (markdown-mode . editorconfig-mode)
)

(provide 'editorconfig-rcp)
;;; Commentary:
;;
;;; editorconfig-rcp.el ends here
