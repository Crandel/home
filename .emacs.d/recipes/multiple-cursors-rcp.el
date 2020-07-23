(use-package multiple-cursors
  :ensure t
  :bind (
  ("C-c C-<right>" . 'mc/mark-next-like-this-word) ; choose same word next
  ("C-c C-<left>" . 'mc/mark-previous-word-like-this) ; choose same word previous
  ("M-n" . 'mc/mark-next-like-this) ; choose char from next line same position
  ("M-m" . 'mc/mark-previous-like-this); choose char from previous line same position
  ("C-c C-_" . 'mc/mark-all-like-this)
  ("C-x M-m" . 'back-to-indentation)
  )
  :config
  (unbind-key "<return>" mc/keymap)
)

(provide 'multiple-cursors-rcp)
