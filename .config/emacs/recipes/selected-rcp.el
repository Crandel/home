;;; selected-rcp.el --- Keymap for when region is active.
;;; Code:
(eval-when-compile (require 'use-package))
(use-package selected
  :ensure t
  :commands selected-global-mode
  :config
  (selected-global-mode)
  :bind (:map selected-keymap
              ("q" . selected-off)
              ("u" . upcase-region)
              ("d" . downcase-region)
              ("<backspace>" . delete-region)
              ("<delete>" . delete-region)))

(provide 'selected-rcp)
;;; Commentary:
;;
;;; selected-rcp.el ends here
