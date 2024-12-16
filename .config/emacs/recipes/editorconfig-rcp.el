;;; editorconfig-rcp.el --- This is an EditorConfig plugin for Emacs.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package editorconfig
  :hook
  (python-ts-mode   . editorconfig-mode)
  (go-ts-mode       . editorconfig-mode)
  (yaml-ts-mode     . editorconfig-mode)
  (sh-ts-mode       . editorconfig-mode)
  (markdown-mode    . editorconfig-mode)
)

(provide 'editorconfig-rcp)
;;; Commentary:
;;
;;; editorconfig-rcp.el ends here
