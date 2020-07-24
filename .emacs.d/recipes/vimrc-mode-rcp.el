;;; vimrc-mode-rcp.el --- Major mode for vimrc files

;;; Commentary:
;; 

;;; Code:

(use-package vimrc-mode
  :ensure t
  :mode (".vim\\(rc\\|peratorrc\\)?$" ".vifm\\(rc\\|peratorrc\\)?$")
)

(provide 'vimrc-mode-rcp)

;;; vimrc-mode-rcp.el ends here
