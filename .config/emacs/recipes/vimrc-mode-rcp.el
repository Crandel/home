;;; vimrc-mode-rcp.el --- Major mode for vimrc files

;;; Code:
(use-package vimrc-mode
  :ensure t
  :defer t
  :mode (".vi\\(mrc\\|mperatorrc\\|fmrc\\|ebrc\\)?$")
)

(provide 'vimrc-mode-rcp)
;;; Commentary:
;;
;;; vimrc-mode-rcp.el ends here
