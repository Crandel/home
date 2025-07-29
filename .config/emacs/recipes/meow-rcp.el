;;; meow-rcp.el --- Yet another modal editing on Emacs

;;; Code:
(eval-when-compile (require 'use-package))
(use-package meow
  :ensure t
  :custom
  (meow-use-clipboard t)
  (meow-keypad-leader-dispatch "C-c")
  :custom-face
  (meow-beacon-indicator      ((t (:inherit font-lock-constant-face))))
  (meow-insert-indicator      ((t (:inherit font-lock-string-face))))
  (meow-normal-indicator      ((t (:inherit success))))
  (meow-search-highlight      ((t (:inherit highlight))))
  (meow-search-indicator      ((t (:inherit lazy-highlight))))
  (meow-beacon-fake-selection ((t (:inherit font-lock-doc-face))))
  :init
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (setq meow--kbd-yank "C-v")
    (setq meow--kbd-backward-char "M-h")
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))

    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     '("h" . "H-h")
     '("l" . "H-l")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("|" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet)
     )

    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     ;; helper function
     '("-" . negative-argument)
     '(";" . meow-reverse)
     ;; Selection
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("F" . meow-find-expand)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("H" . meow-left-expand)
     '("J" . meow-next-expand)
     '("K" . meow-prev-expand)
     '("L" . meow-right-expand)
     '("s" . meow-line)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     ;; movement
     '("h" . meow-left)
     '("j" . meow-next)
     '("k" . meow-prev)
     '("l" . meow-right)
     ;; Other functions
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("c" . meow-change)
     '("d" . meow-kill)
     '("D" . meow-backward-delete)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("m" . meow-join)
     '("M" . meow-indent)
     '("n" . meow-search)
     ;; '("N" . meow-org-motion-mode) define in org-rcp
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-change-char)
     '("R" . meow-replace)
     '("t" . meow-till)
     '("T" . meow-till-expand)
     '("u" . meow-undo)
     '("U" . undo-redo)
     '("v" . meow-end-of-thing)
     '("V" . meow-beginning-of-thing)
     '("x" . meow-delete)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("/" . meow-visit)
     '("'" . repeat)
     '("<backspace>" . meow-kill)
     '("<delete>"    . meow-kill)
     '("<escape>"    . ignore)))
  :config
  (setq meow-replace-state-name-list
   '((org-motion . "O-M")
     (normal . "N")
     (motion . "M")
     (keypad . "K")
     (insert . "I")
     (beacon . "B")))
  (meow-setup)
  (meow-global-mode 1)
  )

(provide 'meow-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; meow-rcp.el ends here
