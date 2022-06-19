;;; markdown-mode-rcp.el --- Markdown mode

;;; Code:
(eval-when-compile (require 'use-package))
(use-package markdown-mode
  :ensure t
  :defer t
  :mode "\\.\\(md\\|mdown\\|markdown\\)\\'"
  :custom
  (markdown-header-scaling t)
  :bind(:map markdown-mode-map
             ("M-n" . mc/mark-next-like-this)
             ("M-m" . mc/mark-previous-like-this))
)

(provide 'markdown-mode-rcp)
;;; Commentary:
;;
;;; markdown-mode-rcp.el ends here
