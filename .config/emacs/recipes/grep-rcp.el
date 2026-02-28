;;; grep-rcp.el --- Packages for ripgrep

;;; Code:
(use-package ripgrep
  :ensure t
  :defer t)

(use-package wgrep
  :ensure t
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit)))

(provide 'grep-rcp)

;;; Commentary:
;;
;;; ripgrep-rcp.el ends here
