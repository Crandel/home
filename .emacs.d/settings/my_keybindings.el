;; Custom keybindings
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y")

;; Multiple cursors
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; Magit
(global-set-key (kbd "C-x C-z") 'magit-status)

(provide 'my_keybindings)
