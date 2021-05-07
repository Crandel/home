;;; multiple-cursors-rcp.el --- Multiple cursors for Emacs.

;;; Code:
(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (
  ("C-c C-<right>" . 'mc/mark-next-like-this-word) ; choose same word next
  ("C-c C-<left>" . 'mc/mark-previous-word-like-this) ; choose same word previous
  ("M-n" . 'mc/mark-next-like-this) ; choose char from next line same position
  ("M-m" . 'mc/mark-previous-like-this); choose char from previous line same position
  ("C-c C-_" . 'mc/mark-all-like-this)
  ("C-x M-m" . 'back-to-indentation)
  )
  :chords (
           ("ee" . mc/keyboard-quit)
  )
  :config
  (unbind-key "<return>" mc/keymap)
)

(provide 'multiple-cursors-rcp)
;;; Commentary:
;;
;;; multiple-cursors-rcp.el ends here
