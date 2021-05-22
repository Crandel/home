;;; use-package-chords-rcp.el --- Add chord support

;;; Code:
(use-package use-package-chords
  :ensure t
  :demand 1
  :custom
  (key-chord-two-keys-delay 0.2)
  (key-chord-one-key-delay  0.1)
  :config
  (key-chord-mode 1)
  (bind-chord " q" 'keyboard-quit)
  (bind-chord " d" 'kill-buffer)
  (bind-chord "wq" 'delete-other-windows)
  (bind-chord " c" 'save-buffers-kill-terminal)
  (bind-chord " x" 'save-buffer)
  (bind-chord " y" 'copy-line)
)

(provide 'use-package-chords-rcp)
;;; Commentary:
;;
;;; use-package-chords-rcp.el ends here
