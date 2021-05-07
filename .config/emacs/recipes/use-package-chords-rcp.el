;;; use-package-chords-rcp.el --- Add chord support

;;; Code:
(use-package use-package-chords
  :ensure t
  :demand 1
  :config
  (key-chord-mode 1)
  (bind-chord "ee" 'keyboard-quit)
  (bind-chord "qq" 'delete-other-windows)
  (bind-chord "xx" 'save-buffer)
  (bind-chord "yy" 'copy-line)
)

(provide 'use-package-chords-rcp)
;;; Commentary:
;;
;;; use-package-chords-rcp.el ends here
