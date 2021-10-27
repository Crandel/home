;;; lua-rcp.el --- Lua mode

;;; Code:
(eval-when-compile (require 'use-package))
(use-package lua-mode
  :ensure t
  :defer t
  :mode "\\.lua\\'"
  :custom
  (lua-indent-level 2)
)

(provide 'lua-rcp)
;;; Commentary:
;;
;;; lua-rcp.el ends here
