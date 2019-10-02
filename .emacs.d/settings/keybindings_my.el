;; Custom keybindings

;; Plugin keybindings

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

;; Yank
(global-unset-key (kbd "C-v"))
(global-unset-key (kbd "C-y"))
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-y") 'scroll-up-command)

(global-set-key (kbd "C-x C-b") 'electric-buffer-list)

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

;; duplicate line
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

;; copy line
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(global-unset-key (kbd "C-c C-k"))
(global-set-key (kbd "C-c C-k") 'copy-line)

;; copy word
(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point)
  )

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "copy thing between beg & end into kill ring"
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
          (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end)))
  )
(defun copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "P")
  (copy-thing 'backward-word 'forward-word arg)
  ;;(paste-to-mark arg)
  )
(global-unset-key (kbd "C-c C-w"))
(global-set-key (kbd "C-c C-w") 'copy-word)

;; delete line
(defun my-delete-line ()
  "Delete text from current position to end of line char."
  (interactive)
  (kill-region
   (move-beginning-of-line 1)
   (save-excursion (move-end-of-line 1) (point)))
  (delete-char 1)
  )

(global-unset-key (kbd "C-d"))
(global-set-key (kbd "C-d") 'my-delete-line)

(global-set-key (kbd "RET") 'newline)
(global-set-key (kbd "M-RET") 'newline-and-indent)

(defvar newline-and-indent t)
;; open new line (vi's o command)
(defun open-next-line ()
  (interactive)
  (end-of-line)
  (open-line 1)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))
(global-set-key (kbd "C-o") 'open-next-line)

;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one. See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))
(global-set-key (kbd "C-c o") 'open-previous-line)


;; tab indent or complete
(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (message (minibufferp))
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(defun my-kill-emacs-with-save ()
  (interactive)
  (save-buffers-kill-terminal "y")
)

(global-unset-key [tab])
(global-set-key [tab] 'tab-indent-or-complete)
(global-set-key (kbd "<backtab>") 'tab-indent-or-complete)
(global-set-key (kbd "C-c b") 'revert-buffer)
(global-set-key (kbd "C-x C-x") 'my-kill-emacs-with-save)
;; C-x r y for paste multiple cursors

(provide 'keybindings_my)
