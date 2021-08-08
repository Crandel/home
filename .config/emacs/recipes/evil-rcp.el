;;; evil-rcp.el --- Evil mode

;;; Code:
(use-package evil-leader
  :ensure t
  :after evil
  :custom
  (evil-leader/leader "<SPC>")
  :config
  (evil-leader/set-key
   "d" 'duplicate-line
   "k" 'kill-buffer
   "l" 'copy-line
   "q" 'keyboard-quit
   "b" 'ibuffer
   "g" 'consult-ripgrep
   "f" 'find-file
   "p" 'consult-buffer
   "/" 'consult-ripgrep-symbol-at-point
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
  ("q"   . nil)
  ("]p" . evil-paste-after-and-indent)
  ("]P" . evil-paste-before-and-indent)
  ("f"   . evil-avy-goto-char)
  :map evil-normal-state-map
  ("q"   . nil)
  ("f"   . evil-avy-goto-char)
  ("M-." . xref-find-definitions)
  ("M-," . xref-find-references)
  ("C-p" . nil)
  ("C-e" . nil)
  :map evil-insert-state-map
  ("C-v" . yank)
  ("C-e" . nil)
  ("C-p" . nil)
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
  (defvar evil-mc-my-cursors-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "A") 'evil-mc-make-cursor-in-visual-selection-end)
      (define-key map (kbd "m") 'evil-mc-make-all-cursors)
      (define-key map (kbd "f") 'evil-mc-make-and-goto-first-cursor)
      (define-key map (kbd "l") 'evil-mc-make-and-goto-last-cursor)
      (define-key map (kbd "n") 'evil-mc-make-and-goto-next-cursor)
      (define-key map (kbd "j") 'evil-mc-make-and-goto-next-match)
      (define-key map (kbd "p") 'evil-mc-make-and-goto-prev-cursor)
      (define-key map (kbd "k") 'evil-mc-make-and-goto-prev-match)
      (define-key map (kbd "h") 'evil-mc-make-cursor-here)
      (define-key map (kbd "I") 'evil-mc-make-cursor-in-visual-selection-beg)
      (define-key map (kbd "A") 'evil-mc-make-cursor-in-visual-selection-end)
      (define-key map (kbd "[") 'evil-mc-make-cursor-move-next-line)
      (define-key map (kbd "]") 'evil-mc-make-cursor-move-prev-line)
      (define-key map (kbd "s") 'evil-mc-pause-cursors)
      (define-key map (kbd "r") 'evil-mc-resume-cursors)
      (define-key map (kbd "N") 'evil-mc-skip-and-goto-next-cursor)
      (define-key map (kbd "J") 'evil-mc-skip-and-goto-next-match)
      (define-key map (kbd "P") 'evil-mc-skip-and-goto-prev-cursor)
      (define-key map (kbd "K") 'evil-mc-skip-and-goto-prev-match)
      (define-key map (kbd "q") 'evil-mc-undo-all-cursors)
      (define-key map (kbd "u") 'evil-mc-undo-last-added-cursor)
      map))
  (evil-define-key* '(normal visual) evil-mc-key-map
    (kbd "m") evil-mc-my-cursors-map
    (kbd "C-n") nil
    (kbd "C-t") nil
    (kbd "M-p") nil
    (kbd "M-n") nil
    (kbd "C-p") nil)
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
     (60 . evil-surround-read-tag)
     (102 . evil-surround-function)))
  :config
  (global-evil-surround-mode 1))

(use-package treemacs-evil
  :ensure t
  :after (evil treemacs)
)

(use-package evil-collection
  :ensure t
  :after evil-mc
  :config
  (delete 'evil-mc evil-collection-mode-list)
  (delete 'company evil-collection-mode-list)
  (evil-collection-init)
)

(provide 'evil-rcp)
;;; Commentary:
;;
;;; evil-rcp.el ends here
