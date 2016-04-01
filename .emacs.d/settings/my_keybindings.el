;; Custom keybindings
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y")
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
(global-set-key (kbd "C-c n") 'mc/mark-next-like-this) ; choose char from next line same position
(global-set-key (kbd "C-c m") 'mc/mark-previous-like-this); choose char from previous line same position
(global-set-key (kbd "C-c /") 'mc/mark-all-like-this)
;; Magit
(global-set-key (kbd "C-x C-z") 'magit-status)
;; Mo-git-blame
(global-set-key (kbd "C-c g") 'mo-git-blame-file)

;; Helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-p") 'helm-mini)
(global-set-key [f10] 'helm-semantic-or-imenu)



(provide 'my_keybindings)
