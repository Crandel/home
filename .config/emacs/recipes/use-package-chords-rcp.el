;;; use-package-chords-rcp.el --- Add chord support

;;; Code:
(use-package use-package-chords
  :ensure t
  :demand 1
  :custom
  (key-chord-two-keys-delay 0.5)
  (key-chord-one-key-delay  0.5)
  :config
  (key-chord-mode 1)
  (bind-chord "ew" 'keyboard-quit)
  (bind-chord "kd" 'kill-buffer)
  (bind-chord "qq" 'delete-other-windows)
  (bind-chord "xx" 'save-buffer)
  (bind-chord "xc" 'save-buffers-kill-terminal)
  (bind-chord "yl" 'copy-line)
)

(provide 'use-package-chords-rcp)
;;; Commentary:
;;
;;; use-package-chords-rcp.el ends here
