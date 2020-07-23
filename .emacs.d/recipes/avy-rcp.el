(use-package avy
  :ensure t
  :bind
  ("C-_" . 'avy-goto-char)
  ("C-/" . 'avy-goto-char)
  ("C-c /" . 'avy-goto-line)
  :custom
  (avy-all-windows 'all-frames)
)

(provide 'avy-rcp)
