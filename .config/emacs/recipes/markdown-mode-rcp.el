;;; markdown-mode-rcp.el --- Markdown mode

;;; Code:
(use-package markdown-mode
  :ensure t
  :defer t
  :mode "\\.\\(md\\|mdown\\|markdown\\)\\'"
  :bind(:map markdown-mode-map
             ("M-n" . mc/mark-next-like-this)
             ("M-m" . mc/mark-previous-like-this))
)

(provide 'markdown-mode-rcp)
;;; Commentary:
;;
;;; markdown-mode-rcp.el ends here
