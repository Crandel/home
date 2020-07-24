(use-package expand-region
  :ensure t
  :bind
  ("C-c ." . er/expand-region)
  :chords
  ("//" . er/expand-region)
)

(provide 'expand-region-rcp)
