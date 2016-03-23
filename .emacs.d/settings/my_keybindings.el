;; Custom keybindings
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y")
;; Moving
(global-unset-key (kbd "M-i"))
(global-set-key (kbd "M-i") 'previous-line)

(global-unset-key (kbd "M-j"))
(global-set-key (kbd "M-j") 'backward-char)

(global-unset-key (kbd "M-k"))
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "C-c k") 'kill-sentence)

(global-unset-key (kbd "M-i"))
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "C-c l") 'downcase-word)

;; Multiple cursors
(global-set-key (kbd "C-c n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c w") 'mc/mark-next-like-this-word)
(global-set-key (kbd "C-c b") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c e") 'mc/mark-previous-word-like-this)
(global-set-key (kbd "C-c a") 'mc/mark-all-like-this)
;; Magit
(global-set-key (kbd "C-x C-z") 'magit-status)

(provide 'my_keybindings)
