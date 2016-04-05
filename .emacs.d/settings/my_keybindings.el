;; Custom keybindings

;; Moving
;;UP
(global-unset-key (kbd "M-i"))
(global-set-key (kbd "M-i") 'previous-line)
;; LEFT
(global-unset-key (kbd "M-j"))
(global-set-key (kbd "M-j") 'backward-char)
;; DOWN
(global-unset-key (kbd "M-k"))
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "C-c k") 'kill-sentence)
;; RIGHT
(global-unset-key (kbd "M-l"))
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "C-c l") 'downcase-word)
;; RIGHT WORD
(global-unset-key (kbd "M-o"))
(global-set-key (kbd "M-o") 'forward-word)
;; LEFT WORD
(global-unset-key (kbd "M-u"))
(global-set-key (kbd "M-u") 'backward-word)
(global-set-key (kbd "C-c u") 'upcase-word)

;; move line up
(defun move-line-up ()
    (interactive)
    (transpose-lines 1)
    (previous-line 2))
(global-set-key [M-S-up] 'move-line-up)

;; move line down
(defun move-line-down ()
    (interactive)
    (next-line 1)
    (transpose-lines 1)
    (previous-line 1))
(global-set-key [M-S-down] 'move-line-down)

;; Multiple cursors
(global-set-key (kbd "C-c .") 'mc/mark-next-like-this-word) ; choose same word next
(global-set-key (kbd "C-c ,") 'mc/mark-previous-word-like-this) ; choose same word previous
(global-set-key (kbd "C-c C-n") 'mc/mark-next-like-this) ; choose char from next line same position
(global-set-key (kbd "C-c C-m") 'mc/mark-previous-like-this); choose char from previous line same position
(global-set-key (kbd "C-c C-_") 'mc/mark-all-like-this)

;; Mo-git-blame
(global-set-key (kbd "C-c g") 'mo-git-blame-current)

;; Helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-p") 'helm-mini)
(global-set-key [f10] 'helm-semantic-or-imenu)


(defun yas/expansion-at-point ()
    (first (yas/current-key)))

(defun company-yasnippet-or-completion ()
  (interactive)
  (if (yas/expansion-at-point)
      (progn (company-abort)
             (yas/expand))
    (company-complete-common)))

(define-key company-active-map [tab] 'company-yasnippet-or-completion)

(defun my-delete-line ()
  "Delete text from current position to end of line char."
  (interactive)
  (delete-region
   (move-beginning-of-line 1)
   (save-excursion (move-end-of-line 1) (point)))
  (delete-char 1)
)
(global-set-key (kbd "C-k") 'my-delete-line)

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)
(global-unset-key (kbd "C-x C-d"))
(global-set-key (kbd "C-x C-d") 'duplicate-line)


(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-x u") 'suspend-frame)

(global-set-key (kbd "C-c r") '(redo undo-tree-redo ergoemacs-redo))
(provide 'my_keybindings)
