;;; evil-rcp.el --- Evil mode

;;; Code:
(eval-when-compile (require 'use-package))
(use-package evil-leader
  :ensure t
  :after evil
  :custom
  (evil-leader/leader "<SPC>")
  :config
  (evil-leader/set-key
   "a" 'beginning-of-defun
   "e" 'end-of-defun
   "b" 'ibuffer
   "c" 'vd/find-in-config
   "d" 'vd/duplicate-line
   "f" 'find-file
   "k" 'kill-buffer
   "l" 'vd/copy-line
   "q" 'keyboard-quit
   "w" 'evil-window-map
   )
  (evil-leader/set-key-for-mode
    'evil-visual-state-map
        "q" 'evil-exit-visual-state
    )
  (global-evil-leader-mode)
)

(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-normal-state-tag   (propertize "<N>" 'face '((:background "DarkGoldenrod2" :foreground "black")))
        evil-emacs-state-tag    (propertize "<E>" 'face '((:background "SkyBlue2"       :foreground "black")))
        evil-insert-state-tag   (propertize "<I>" 'face '((:background "chartreuse3"    :foreground "black")))
        evil-replace-state-tag  (propertize "<R>" 'face '((:background "chocolate"      :foreground "black")))
        evil-motion-state-tag   (propertize "<M>" 'face '((:background "plum3"          :foreground "black")))
        evil-visual-state-tag   (propertize "<V>" 'face '((:background "gray"           :foreground "black")))
        evil-operator-state-tag (propertize "<O>" 'face '((:background "sandy brown"    :foreground "black")))
        evil-undo-system                   'undo-redo
        evil-want-C-i-jump                 nil
        evil-want-C-d-scroll               nil
        evil-want-integration              t
        evil-want-keybinding               nil
        evil-disable-insert-state-bindings t
        evil-normal-state-cursor           '(hollow . 2)
        evil-want-fine-undo                t)
  :config
  (evil-define-command evil-paste-before-and-indent
    (count &optional register yank-handler)
    "Pastes the latest yanked text before point
and gives it the same indentation as the surrounding code.
The return value is the yanked text."
    (interactive "P<x>")
    (evil-with-single-undo
      (end-of-line)
      (newline-and-indent)
      (let ((text (evil-paste-before count register yank-handler)))
        (evil-indent (evil-get-marker ?\[) (evil-get-marker ?\]))
        text)))

  (evil-define-command evil-paste-after-and-indent
    (count &optional register yank-handler)
    "Pastes the latest yanked text behind point
and gives it the same indentation as the surrounding code.
The return value is the yanked text."
    (interactive "P<x>")
    (evil-with-single-undo
      (end-of-line)
      (newline-and-indent)
      (let ((text (evil-paste-after count register yank-handler)))
        (evil-indent (evil-get-marker ?\[) (evil-get-marker ?\]))
        text)))
  (evil-mode 1)
  :bind (
  ("C-x e" . evil-mode)
  :map evil-motion-state-map
  ("]P" . evil-paste-before-and-indent)
  ("]p" . evil-paste-after-and-indent)
  ("f"  . evil-avy-goto-char)
  ("q"  . nil)
  :map evil-normal-state-map
  ("C-e" . nil)
  ("C-p" . nil)
  ("S-<up>"   . scroll-down-command)
  ("S-<down>" . scroll-up-command)
  ("M-," . xref-find-references)
  ("M-." . xref-find-definitions)
  ([remap yank-pop] . nil)
  ("f"   . evil-avy-goto-char)
  ("g["  . text-scale-decrease)
  ("g]"  . text-scale-increase)
  ("gc"  . comment-dwim)
  ("q"   . nil)
  :map evil-insert-state-map
  ("C-v" . yank)
  ("C-e" . nil)
  ("C-p" . nil)
  :map evil-window-map
  ("u" . winner-undo)
  ("r" . winner-redo)
  )
  :chords
  ("jk" . evil-force-normal-state)
)

(use-package evil-ex
  :after evil
  :config
  (evil-ex-define-cmd "q[uit]" 'save-buffers-kill-terminal)
  (evil-ex-define-cmd "k[ill]" 'kill-buffer)
  :bind (
         :map evil-motion-state-map
         (";" . 'evil-ex)
         (":" . 'evil-repeat-find-char)
        )
)

(use-package evil-mc
  :ensure t
  :after evil
  :custom
  (evil-mc-one-cursor-show-mode-line-text nil)
  :config
  (evil-define-key* '(normal visual) evil-mc-key-map
    (kbd "m")   nil
    (kbd "C-n") nil
    (kbd "C-t") nil
    (kbd "M-p") nil
    (kbd "M-n") nil
    (kbd "C-p") nil)
  :bind (
  ("C-c q" . evil-mc-undo-all-cursors)
  :map evil-motion-state-map
  ("grq" . evil-mc-undo-all-cursors)
  :map evil-normal-state-map
  ("grq" . evil-mc-undo-all-cursors)
  )
  :hook
  (evil-local-mode . evil-mc-mode)
)

(use-package evil-surround
  :ensure t
  :after evil
  :custom
  (evil-surround-pairs-alist
   '((40 "(" . ")")
     (91 "[" . "]")
     (123 "{" . "}")
     (41 "(" . ")")
     (93 "[" . "]")
     (125 "{" . "}")
     (35 "#{" . "}")
     (98 "(" . ")")
     (66 "{" . "}")
     (62 "<" . ">")
     (116 . evil-surround-read-tag)
     (60  . evil-surround-read-tag)
     (102 . evil-surround-function)))
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :ensure t
  :after evil-mc
  :config
  (delete 'evil-mc evil-collection-mode-list)
  (evil-collection-init)
  (evil-collection-translate-key
    'normal
    '(dired-mode-map dired-sidebar-mode-map)
    "H" "^"
    ;; Set this to t to make this swap the keys everytime
    ;; this expression is evaluated.
    :destructive t)
)

(provide 'evil-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; evil-rcp.el ends here
