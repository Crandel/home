;;; evil-rcp.el --- Evil mode

;;; Code:
(eval-when-compile (require 'use-package))
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
  ("C-." . nil)
  ([remap yank-pop] . nil)
  ("f"   . evil-avy-goto-char)
  ("g["  . text-scale-decrease)
  ("g]"  . text-scale-increase)
  ("gJ"  . vd/move-line-down)
  ("gK"  . vd/move-line-up)
  ("gc"  . comment-dwim)
  ("gl"  . consult-imenu)
  ("gp"  . projectile-command-map)
  ("gs"  . magit-status)
  ("gtd" . consult-dir)
  ("gtf" . consult-lsp-file-symbols)
  ("gtj" . consult-dir-jump-file)
  ("gtm" . consult-lsp-symbols)
  ("gtn" . chezmoi-find)
  ("gtt" . journalctl)
  ("gtv" . consult-yank-from-kill-ring)
  ("gtw" . chezmoi-write)
  ("g."  . vertico-repeat)
  ("q"   . nil)
  :map evil-insert-state-map
  ("C-v" . yank)
  ("C-e" . nil)
  ("C-p" . nil)
  :map evil-window-map
  ("u" . winner-undo)
  ("r" . winner-redo)
  )
  :hook
  (go-ts-mode . (lambda ()
    (evil-define-key 'normal 'local (kbd "gbb")   'go-dap-setup)
    (evil-define-key 'normal 'local (kbd "gbh")   'go-root-setup)
    (evil-define-key 'normal 'local (kbd "gbt")   'dap-breakpoint-toggle)
  ))
  (python-ts-mode . (lambda()
    (evil-define-key 'normal 'local (kbd "gbp") 'run-python)
    (evil-define-key 'normal 'local (kbd "gbs") 'python-shell-send-statement)
    (evil-define-key 'normal 'local (kbd "gbf") 'python-shell-send-file)
    (evil-define-key 'normal 'local (kbd "gbr") 'python-shell-send-region)
    (evil-define-key 'normal 'local (kbd "gbb") 'python-shell-send-buffer)
  ))
  (org-mode . (lambda ()
    (evil-define-key 'normal 'local "gto" 'vd/org-toggle-emphasis)
    (evil-define-key 'normal 'local "gti" 'org-insert-structure-template)
    (evil-define-key 'normal 'local "gta" 'org-babel-remove-result)
  ))
)

(use-package evil-leader
  :ensure t
  :after evil
  :custom
  (evil-leader/leader "<SPC>")
  :config
  (evil-leader/set-key
   "a"  'beginning-of-defun
   "bb" 'ibuffer
   "bk" 'kill-buffer
   "d"  'dired-sidebar-toggle-sidebar
   "e"  'end-of-defun
   "f"  'find-file
   "i"  'consult-imenu-multi
   "mc" 'vd/copy-line
   "md" 'vd/duplicate-line
   "ms" 'sort-lines
   "p"  'consult-buffer
   "q"  'keyboard-quit
   "sj" 'scroll-up ;; scroll-up moves content down
   "sk" 'scroll-down
   "v"  'vd/find-in-config
   "w"  'evil-window-map
   "x"  'delete-other-windows
   "z"  'evil-repeat
   ","  'indent-region
   "."  'vertico-repeat
   "/"  'consult-ripgrep

   "h"  'evil-window-left
   "j"  'evil-window-down
   "k"  'evil-window-up
   "l"  'evil-window-right
   "<left>"  'evil-window-left
   "<down>"  'evil-window-down
   "<up>"    'evil-window-up
   "<right>" 'evil-window-right
   )
  (evil-leader/set-key-for-mode
    'evil-visual-state-map
        "q" 'evil-exit-visual-state
    )
  (global-evil-leader-mode)
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
    (kbd "grf")   nil
    (kbd "C-n") nil
    (kbd "C-t") nil
    (kbd "M-p") nil
    (kbd "M-n") nil
    (kbd "C-p") nil)
  (eval-after-load "evil-mc"
  '(progn
     (evil-define-key* '(normal visual motion) 'global (kbd "gr") nil)
     ))
  (defhydra vd-mc-hydra (:foreign-keys run
                           :hint nil
                           :pre (evil-mc-pause-cursors))
   "
   ^Match^            ^Line-wise^           ^Manual^
   ^^^^^^--------------------------------------
   _Z_: match all     _J_: make & go down   _z_: toggle cursor here
   _m_: make & next   _K_: make & go up     _r_: remove last
   _M_: make & prev   _]_: goto next cursor _R_: remove all
   _n_: skip & next   _[_: goto prev cursor _q_: quit
   _N_: skip & prev

   Current pattern: %`evil-mc-pattern
   "
    ("Z" #'evil-mc-make-all-cursors)
    ("m" #'evil-mc-make-and-goto-next-match)
    ("M" #'evil-mc-make-and-goto-prev-match)
    ("n" #'evil-mc-skip-and-goto-next-match)
    ("N" #'evil-mc-skip-and-goto-prev-match)
    ("]" #'evil-mc-make-and-goto-next-cursor)
    ("[" #'evil-mc-make-and-goto-prev-cursor)
    ("z" #'evil-mc-make-cursor-here)
    ("J" #'evil-mc-make-cursor-move-next-line)
    ("K" #'evil-mc-make-cursor-move-prev-line)
    ("r" #'evil-mc-undo-last-added-cursor)
    ("R" #'evil-mc-undo-all-cursors "quit" :exit t)
    ("q" #'evil-mc-resume-cursors "quit" :exit t)
    ("<escape>" #'evil-mc-resume-cursors "quit" :exit t)
    )
  (evil-define-key* '(normal visual) 'global  (kbd "m") 'vd-mc-hydra/body)
  :bind (
  ("C-c q" . evil-mc-undo-all-cursors)
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
  :after evil
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

(use-package evil-goggles
  :after evil
  :ensure t
  :config
  (evil-goggles-mode))

(provide 'evil-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; evil-rcp.el ends here
