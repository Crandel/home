;;; multiple-cursors-rcp.el --- Multiple cursors for Emacs.

;;; Code:
(eval-when-compile (require 'use-package))
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
  :hook
  (multiple-cursors-mode . (lambda()
                             (unbind-key "<return>" mc/keymap)
                             (key-chord-define mc/keymap "ew" 'mc/keyboard-quit)
                             ))
)

(provide 'multiple-cursors-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; multiple-cursors-rcp.el ends here
