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
   "b" 'consult-recent-file
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
  (evil-mode 1)
  ;:hook
  ;(evil-local-mode . turn-on-undo-tree-mode)
  :bind (
  ("C-x e" . evil-mode)
  :map evil-motion-state-map
  ("q"   . nil)
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
  (evil-define-key* '(normal visual) evil-mc-cursors-map
    "j" 'evil-mc-make-and-goto-next-match
    "k" 'evil-mc-make-and-goto-prev-match
    "J" 'evil-mc-make-cursor-move-next-line
    "K" 'evil-mc-make-cursor-move-prev-line
    )
  (evil-define-key* '(normal visual) evil-mc-key-map
    (kbd "m") evil-mc-cursors-map
    (kbd "C-p") nil)
  ;; (key-chord-define evil-mc-cursors-map "wq" 'evil-mc-undo-all-cursors)
  ;; (key-chord-define evil-mc-cursors-map "jk" 'evil-force-normal-state)
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
  :after evil
  :config
  (evil-collection-init)
)

(provide 'evil-rcp)
;;; Commentary:
;;
;;; evil-rcp.el ends here
