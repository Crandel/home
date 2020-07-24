(use-package use-package-chords
  :ensure t
  :config
  (key-chord-mode 1)
  (bind-chord "yy" 'copy-line)
  (bind-chord "xx" 'save-buffer)
  (bind-chord "qq" 'delete-other-windows)
  (bind-chord "bb" 'electric-buffer-list)
)

(provide 'use-package-chords-rcp)
