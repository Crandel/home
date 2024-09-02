;;; which-key-rcp.el --- Add help for keymapping

;;; Code:
(eval-when-compile (require 'use-package))
(use-package which-key
  :defer 1
  :commands (which-key--show-keymap which-key--hide-popup-ignore-command)
  :custom
  (which-key-show-transient-maps t)
  :config
  (which-key-mode))

(provide 'which-key-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; which-key-rcp.el ends here
