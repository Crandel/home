(use-package use-package-chords
  :ensure t
  :config
  (key-chord-mode 1)
  (bind-chord "yy" 'copy-line)
  (bind-chord "xx" 'save-buffer)
  (bind-chord "qq" 'delete-other-windows)
)

(provide 'use-package-chords-rcp)
