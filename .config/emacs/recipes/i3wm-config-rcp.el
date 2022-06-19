;;; i3wm-config-rcp.el --- An expansion of conf-mode to bring proper syntax highlighting to your i3wm config

;;; Code:
(eval-when-compile (require 'use-package))
(use-package i3wm-config-mode
  :ensure t
  :mode
  ("\\.i3/config\\'" "\\sway/config\\'" "\\sway/config\\.tmpl\\'" "\\.sway\\'")
)

(provide 'i3wm-config-rcp)
;;; Commentary:
;;
;;; i3wm-config-rcp.el ends here
