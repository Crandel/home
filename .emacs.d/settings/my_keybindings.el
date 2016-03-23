;; Custom keybindings
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y")
;; Movingi
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

;; Multiple cursors
(global-set-key (kbd "C-c .") 'mc/mark-next-like-this-word) ; choose same word next
(global-set-key (kbd "C-c ,") 'mc/mark-previous-word-like-this) ; choose same word previous
(global-set-key (kbd "C-c n") 'mc/mark-next-like-this) ; choose char from next line same position
(global-set-key (kbd "C-c m") 'mc/mark-previous-like-this); choose char from previous line same position
(global-set-key (kbd "C-c /") 'mc/mark-all-like-this)
;; Magit
(global-set-key (kbd "C-x C-z") 'magit-status)

(provide 'my_keybindings)
