;;; vimrc-mode-rcp.el --- Major mode for vimrc files

;;; Code:
(use-package vimrc-mode
  :ensure t
  :defer t
  :mode (".vim\\(rc\\|peratorrc\\)?$" ".vifm\\(rc\\|peratorrc\\)?$")
)

(provide 'vimrc-mode-rcp)
;;; Commentary:
;;
;;; vimrc-mode-rcp.el ends here
