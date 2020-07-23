(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode t)
  :custom
  (undo-tree-visualizer-diff t)
  :bind (
  ("C-z" . undo-tree-visualize)
  ("C-c C-z" . undo-tree-redo)
  (:map undo-tree-map
        ("C-_" . nil)
        ("C-?" . nil)
        ("C-/" . nil)
        ("M-_" . nil)
        )
  )
  :chords (
  ("zz" . undo-tree-visualize)
  ("zx" . undo-tree-undo))
)

(provide 'undo-tree-rcp)
