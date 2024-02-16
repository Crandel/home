;;; functions_my.el --- File for custom code and functions

;;; Code:

;; Moving
;;;###autoload
(defun vd/move-line-up ()
  "Move line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

;;;###autoload
(defun vd/move-line-down ()
  "Move line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

;;;###autoload
(defun vd/duplicate-line()
  "Duplicate whole line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank)
  )

;;;###autoload
(defun vd/copy-line (arg)
  "Copy lines (as many as prefix ARG) in the kill ring."
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

;;;###autoload
(defun vd/get-point (symbol &optional arg)
  "Copy word current SYMBOL belongs.  With additional ARG."
  (funcall symbol arg)
  (point)
  )

;;;###autoload
(defun vd/copy-thing (begin-of-thing end-of-thing &optional arg)
  "Copy thing between BEGIN-OF-THING & END-OF-THING into kill ring.
With optional ARG."
  (save-excursion
    (let ((beg (vd/get-point begin-of-thing 1))
          (end (vd/get-point end-of-thing arg)))
      (copy-region-as-kill beg end)))
  )

;;;###autoload
(defun vd/copy-word (&optional arg)
  "Copy words at point into KILL-RING.  With optional ARG."
  (interactive "P")
  (vd/copy-thing 'backward-word 'forward-word arg)
  ;;(paste-to-mark arg)
  )

;;;###autoload
(defun vd/delete-line ()
  "Delete text from begin to end of line char."
  (interactive)
  (kill-region
   (move-beginning-of-line 1)
   (save-excursion (move-end-of-line 1) (point)))
  (delete-char 1)
  )


(defvar newline-and-indent t)

;;;###autoload
(defun vd/open-next-line ()
  "Open new line (vi's o command)."
  (interactive)
  (end-of-line)
  (open-line 1)
  (forward-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

;;;###autoload
(defun vd/open-previous-line (arg)
  "Open a new line before the current one.
See also `newline-and-indent'.  Behave like vi's O command.  With optional ARG."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

;;;###autoload
(defun vd/save-all-buffers ()
  "Save all buffers."
  (interactive)
  (save-some-buffers t)
)

;;;###autoload
(defun vd/kill-emacs-with-save ()
  "Kill terminal."
  (interactive)
  (save-buffers-kill-terminal "y")
)

;;;###autoload
(defun vd/kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
;; C-x r y for paste multiple cursors

;;;###autoload
(defun vd/find-in-directory (&optional dir)
  "Find files in DIR."
  (interactive)
  (let ((dir (or dir "~")))
        (setq default-directory dir)
        (call-interactively 'find-file)
    )
)

;;;###autoload
(defun vd/find-in-config ()
  "Find in config directory."
  (interactive)
  (vd/find-in-directory "~/.config/")
)
;;;###autoload
(defun vd/delete-this-file (&optional path force-p)
  "Delete PATH, kill its buffers.
If PATH is not specified, default to the current buffer's file.
If FORCE-P, delete without confirmation."
  (interactive
  (list (buffer-file-name (buffer-base-buffer))
        current-prefix-arg))
    (let* ((path (or path (buffer-file-name (buffer-base-buffer))))
         (short-path (abbreviate-file-name path)))
    (unless (and path (file-exists-p path))
      (user-error "Buffer is not visiting any file"))
    (unless (file-exists-p path)
      (error "File doesn't exist: %s" path))
    (unless (or force-p (y-or-n-p (format "Really delete %S?" short-path)))
      (user-error "Aborted"))
    (let ((buf (current-buffer)))
      (unwind-protect
          (progn (delete-file path t) t)
        (if (file-exists-p path)
            (error "Failed to delete %S" short-path)
          (kill-buffer buf)
          (message "Deleted %S" short-path))))))

;;;###autoload
(defun vd/highlight-todos ()
    (font-lock-add-keywords nil
                            '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))))


(provide 'functions_my)
;;; Commentary:
;;
;;; functions_my.el ends here
