;;; base-rcp.el --- Emacs default configuration

;;; Commentary:
;; 

;;; Code:

(use-package emacs
  :defer t
  :init
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (when (display-graphic-p)
    (tool-bar-mode    -1)
    (scroll-bar-mode  -1)
    ;; Fringe settings
    (fringe-mode '(8 . 0))
    (setq-default indicate-buffer-boundaries 'left)
    ;; Cursor
    (setq-default cursor-type 'bar)
    (set-cursor-color "#BE81F7")
    (blink-cursor-mode 1)
    )
  (tooltip-mode  -1)
  (menu-bar-mode -1)
  (cl-loop
   for from across "йцукенгшщзхїфівапролджєячсмитьбюЙЦУКЕНГШЩЗХЇФІВАПРОЛДЖ\ЄЯЧСМИТЬБЮ№"
   for to   across "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"
   do
   (eval `(define-key local-function-key-map
            (kbd ,(concat "C-"
                          (string from)))
            (kbd ,(concat "C-"
                          (string to)))))
   (eval `(define-key local-function-key-map
            (kbd ,(concat "M-"
                          (string from)))
            (kbd ,(concat "M-"
                          (string to)))))
   (eval `(define-key local-function-key-map
            (kbd ,(string from))
            (kbd ,(string to)))))
  :config
  (set-frame-font "Hack 18" "Font settings")
  (delete-selection-mode t)
  (column-number-mode    t)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (global-font-lock-mode 1)
  :custom
  (shell-file-name        "/bin/zsh" "Set zsh as default shell")
  (inhibit-startup-screen t "Don't show splash screen")
  (use-dialog-box         nil)
  (ring-bell-function     'ignore)
  (frame-title-format     "%b" "Display the name of the current buffer in the title bar")
  (display-line-numbers      t)
  (display-time-24hr-format  t)
  (display-time-mode         t)
  (size-indication-mode      t)
  (indent-tabs-mode          nil "Indent settings")
  (tab-width                 4)
  (tab-always-indent         nil)
  (c-basic-offset            2)
  (sh-basic-offset           2)
  (sh-indentation            2)
  (scala-basic-offset        2)
  (java-basic-offset         2)
  (standart-indent           2)
  (lisp-body-indent          2)
  (rust-indent-offset        4)
  (js-indent-level           2)
  (indent-line-function      'insert-tab "End Indent settings")
  (scroll-step               1 "Scrolling settings")
  (scroll-margin             10)
  (scroll-conservatively     10000)
  (next-line-add-newlines    nil)
  (ad-redefinition-action    'accept)
  (max-mini-window-height    0.5)
  (checkdoc-spellcheck-documentation-flag t)
  (nxml-attribute-indent 2)
  :bind
  ("M-i" . previous-line)
  ("M-j" . backward-char)
  ("M-k" . next-line)
  ("C-c k" . kill-sentence)
  ("M-l" . forward-char)
  ("C-c l" . downcase-word)
  ("M-o" . forward-word)
  ("M-u" . backward-word)
  ("C-c u" . upcase-word)
  ("C-v" . yank)
  ("C-y" . scroll-up-command)
  ("RET" . newline)
  ("M-RET" . newline-and-indent)
  ("<backtab>" . tab-indent-or-complete)
  ("C-c b" . revert-buffer)
  ("C-x C-x" . my-kill-emacs-with-save)
  ("C-x a s" . sort-lines)
)

(use-package recentf
  :defer t
  :custom
  (recentf-max-saved-items 1500
   recentf-max-menu-items  150
   recentf-save-file (concat user-emacs-directory ".recentf"))
  :config
  (recentf-mode t)
  :diminish nil)

(use-package imenu
  :defer t
  :custom
  (imenu-auto-rescan      t)
  (imenu-use-popup-menu   nil)
)

(use-package semantic
  :defer t
  :config
  (semantic-mode 1)
  :custom
  (semantic-which-function-use-color t)
)

(use-package saveplace
  :defer t
  :init
  (save-place-mode 1)
  :custom
  (save-place-file "~/.emacs.d/saved-places")
  (save-place-forget-unreadable-files t)
)

(use-package electric
  :defer t
  :config
  (electric-pair-mode     -1)
  (electric-indent-mode   -1)
  :bind
  ("C-x C-b" . 'electric-buffer-list)
)

(use-package files
  :defer t
  :custom
  (auto-save-default              t)
  (auto-save-interval             0)
  (auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
  (auto-save-list-file-name       nil)
  (auto-save-list-file-prefix     emacs-tmp-dir)
  (auto-save-timeout              3)
  (backup-directory-alist         `((".*" . ,emacs-tmp-dir)))
  (backup-inhibited               t)
  (create-lockfiles               nil "Disable lockfiles .#filename")
  (delete-old-versions            t   "Don't ask to delete excess backup versions.")
  (make-backup-files              t)
  (version-control                t   "Use version numbers for backups.")
  (vc-make-backup-files           t   "Emacs never backs up versioned files")
  (kept-new-versions              5   "Number of newest versions to keep.")
  (kept-old-versions              0   "Number of oldest versions to keep.")
)

(use-package mule
  :defer t
  :config
  (prefer-coding-system        'utf-8)
  (set-keyboard-coding-system  'utf-8-unix)
  (set-language-environment    'UTF-8)
  (set-selection-coding-system 'utf-8)
  (set-terminal-coding-system  'utf-8)
  :custom
  (buffer-file-coding-system 'utf-8)
  (file-name-coding-system   'utf-8)
  (coding-system-for-read    'utf-8)
)

(use-package shell
  :defer t
  :functions add-mode-line-dirtrack
  :config
  (defun add-mode-line-dirtrack ()
    (add-to-list 'mode-line-buffer-identification
                 '(:propertize (" " default-directory " ") face dired-directory)))
  (add-hook 'shell-mode-hook 'add-mode-line-dirtrack)
)

(use-package select
  :defer t
  :custom
  (save-interprogram-paste-before-kill t)
  (select-enable-clipboard             t)
  (selection-coding-system             'utf-8)
  (wl-copy-process                     nil)
  :init
  (defun wl-copy (text)
    (setq wl-copy-process (make-process :name "wl-copy"
                                        :buffer nil
                                        :command '("wl-copy" "-f" "-n")
                                        :connection-type 'pipe))
    (process-send-string wl-copy-process text)
    (process-send-eof wl-copy-process))
  (defun wl-paste ()
    (if (and wl-copy-process (process-live-p wl-copy-process))
        nil ; should return nil if we're the current paste owner
      (shell-command-to-string "wl-paste -n | tr -d \r")))
  :functions (wl-copy wl-paste)
  :custom
  (interprogram-cut-function 'wl-copy)
  (interprogram-paste-function 'wl-paste)
)

(use-package isearch
  :defer t
  :custom
  (search-highlight          t "Highlight search results")
  (query-replace-highlight   t)
  (auto-window-vscroll       nil)
  (bidi-display-reordering   nil)
)

(use-package whitespace
  :defer t
  :init
  (global-whitespace-mode t)
  :custom-face
  (whitespace-style-face '(trailing spaces lines-tail empty indentation::tab
                           indentation::space tabs newline space-mark tab-mark newline-mark))
  (whitespace-empty ((t (:foreground "sienna"))))
  (whitespace-hspace ((t (:background "grey24" :foreground "MistyRose4"))))
  (whitespace-indentation ((t (:foreground "DarkOrchid4"))))
  (whitespace-newline ((t (:foreground "dark green" :weight normal))))
  (whitespace-space ((t (:foreground "DarkOrchid4"))))
  (whitespace-space-after-tab ((t (:foreground "firebrick"))))
  (whitespace-space-before-tab ((t (:foreground "firebrick"))))
  (whitespace-tab ((t (:foreground "magenta"))))
  (whitespace-trailing ((t (:foreground "yellow" :weight bold))))
  :custom
  (whitespace-global-modes '(not magit-diff-mode))
  (whitespace-line-column 130)
  (whitespace-display-mappings
   ;; all numbers are Unicode codepoint in decimal. ⁖ (insert-char 182 1)
   '(
     (space-mark 32 [183] [46]) ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
     (newline-mark 10 [8617 10]) ; 10 LINE FEED
     (lines-tail 10 [8617 10]) ; 10 LINE FEED
     (tab-mark 9 [8594 9] [183 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
     ))
)

(use-package window
  :defer t
  :custom
  (split-height-threshold  nil)
  (split-width-threshold   0)
  :config
  (if (equal nil (equal major-mode 'org-mode))
      (windmove-default-keybindings 'meta))
)

(use-package paren
  :defer t
  :init
  (show-paren-mode 2)
  :custom-face
  (show-paren-match ((t (:background "#1d2021" :foreground "#def" :weight extra-bold))))
  :custom
  (show-paren-delay 0.2)
  (show-paren-style 'expression)
)

(use-package ispell
  :defer t
  :custom
  (ispell-local-dictionary-alist
   '(("russian"
      "[АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюяіїєґ’A-Za-z]"
      "[^АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюяіїєґ’A-Za-z]"
      "[-']"  nil ("-d" "uk_UA,ru_RU,en_US") nil utf-8)))
  (ispell-program-name "hunspell")
  ;(ispell-dictionary "russian")
  (ispell-really-aspell nil)
  (ispell-really-hunspell t)
  (ispell-encoding8-command t)
  (ispell-silently-savep t)
)

(use-package compile
  :custom
  (compilation-always-kill     t)
  (compilation-disable-input   t)
  (compilation-window-height   10)
)

(use-package dired
  :custom
  (dired-dwim-target t "guess a target directory")
  (dired-listing-switches "-ahlF --time-style=long-iso --group-directories-first")
)

(use-package ediff-util
  :custom
  (ediff-merge-split-window-function 'split-window-vertically)
)

(use-package windmove
  :defer t
  :init
  (windmove-default-keybindings)
)

(provide 'base-rcp)

;;; base-rcp.el ends here
