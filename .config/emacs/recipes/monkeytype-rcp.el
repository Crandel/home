;;; monkeytype-rcp.el --- Typing game/tutor for Emacs inspired by github.com/Miodec/monkeytype.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package monkeytype
  :ensure t
  :preface
  (defun vd/monkeytype-load-and-start (arg &optional file-name num-words)
    (interactive "p")
    (when (/= arg 1)
      (setf file-name (read-file-name "Insert name of file with words: "))
      (setf num-words (read-number "Insert number of words you require: " 50)))

    (let ((res '())
          (final-buffer "*Monkeytype-words*")
          (true-num-words (or num-words 50))
          (num-buffer-words nil)
          (indices nil))

      (with-temp-buffer
        (insert-file-contents
         (or file-name
             (expand-file-name
              "words.txt"
              monkeytype-directory)))

        (setq num-buffer-words
              (count-words
               (point-min)
               (point-max)))
        (setq indices
              (sort
               (cl-loop for i from 0 below true-num-words
                        collect
                        (random (- num-buffer-words i)))
               '<))
        (setq res
              (cl-loop repeat true-num-words
                       for idx in indices
                       collect
                       (progn
                         (goto-char (point-min))
                         (forward-word idx)
                         (let ((word-to-return
                                (string-trim
                                 (buffer-substring-no-properties
                                  (point)
                                  (progn (forward-word) (point))))))
                           (kill-word -1)
                           word-to-return)))))

      (with-current-buffer (get-buffer-create final-buffer)
        (erase-buffer)
        (insert (mapconcat 'identity res " ")))
      (switch-to-buffer final-buffer)
      (monkeytype-buffer)))
  :custom
  (monkeytype-directory (expand-file-name "monkeytype" user-emacs-directory))
  (monkeytype-dowcase   nil)
  :hook
  (monkeytype-mode . (lambda()
                       (setq word-wrap t)
                       (text-scale-increase 3)
                       (centered-window-mode)
                       (centered-cursor-mode-set-explicitly)
                       (meow-insert)))
  )

(use-package centered-cursor-mode
  :ensure t
  :demand
)

(use-package centered-window
  :ensure t
  :custom
  (cwm-use-vertical-padding  t)
  (cwm-frame-internal-border 80)
)

(provide 'monkeytype-rcp)
;;; Commentary:
;;
;;; monkeytype-rcp.el ends here
