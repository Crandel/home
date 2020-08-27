;;; base-rcp.el --- Emacs default configuration

;;; Code:
(use-package emacs
  :init
  (setq initial-frame-alist
        '(
          (alpha 100 100)
          (cursor-color . "#BE81F7")
          (cursor-type . 'bar)
          (font . "Hack Nerd Font-16")
          (tool-bar-lines . 0)
          (vertical-scroll-bars . right)
          ))
  (setq default-frame-alist
        '(
          (alpha 80 80)
          (cursor-type . 'bar)
          (cursor-color . "#BE81F7")
          (font . "Hack Nerd Font-16")
          (tool-bar-lines . 0)
          (vertical-scroll-bars . right)
          ))
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (tool-bar-mode    -1)
  (tooltip-mode  -1)
  (menu-bar-mode -1)
  (scroll-bar-mode  1)
  (blink-cursor-mode 1)
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
  (set-frame-font            "Hack Nerd Font-16" "Font settings")
  (column-number-mode        t)
  (defalias 'yes-or-no-p     'y-or-n-p)
  (global-font-lock-mode     1)
  :custom
  (split-height-threshold    nil "Minimum height for splitting windows vertically.")
  (split-width-threshold     0   "Minimum height for splitting windows horizontally.")
  (bidi-display-reordering   nil "Never reorder bidirectional text for display in the visual order.")
  (inhibit-startup-screen    t   "Don't show splash screen")
  (use-dialog-box            nil "Non-nil means mouse commands use dialog boxes to ask questions.")
  (ring-bell-function        'ignore)
  (frame-title-format        "%b %I %P %l" "Display the name of the current buffer in the title bar")
  (display-line-numbers      t)
  (display-time-24hr-format  t)
  (display-time-mode         t)
  (size-indication-mode      t)
  (indent-tabs-mode          nil "Indent settings")
  (tab-width                 4)
  (tab-always-indent         nil)
  (c-basic-offset            2)
  (java-basic-offset         2)
  (standart-indent           2)
  (lisp-body-indent          2)
  (js-indent-level           2)
  (indent-line-function      'insert-tab "End Indent settings")
  (scroll-step               1 "Scrolling settings")
  (scroll-margin             10)
  (scroll-conservatively     10000)
  (next-line-add-newlines    nil)
  (ad-redefinition-action    'accept)
  (max-mini-window-height    0.5)
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
  ("C-c b" . revert-buffer)
  ("C-x a s" . sort-lines)
  ("C-x a d" . delete-trailing-whitespace)
  ;; keybindings from functions_my.el file
  ([M-S-up] . move-line-up)
  ([M-S-down] . move-line-down)
  ("C-x C-d" . duplicate-line)
  ("C-c C-k" . copy-line)
  ("C-c C-w" . copy-word)
  ("C-d" . my-delete-line)
  ("C-o" . open-next-line)
  ;; ("<tab>" . tab-indent-or-complete)
  ("C-x C-x" . my-kill-emacs-with-save)
  ("C-c o" . open-previous-line)
)

(use-package autorevert
  :demand t
  :config
  (auto-revert-mode t)
)

(use-package compile
  :custom
  (compilation-always-kill     t)
  (compilation-disable-input   t)
  (compilation-window-height   10)
)

(use-package delsel
  :demand t
  :config
  (delete-selection-mode t)
)

(use-package dired
  :custom
  (dired-dwim-target t      "guess a target directory")
  (dired-auto-revert-buffer t)
  (dired-listing-switches   "-ahlF --time-style=long-iso --group-directories-first")
)

(use-package ediff-util
  :custom
  (ediff-forward-word-function       'forward-char)
  (ediff-highlight-all-diffs         t)
  (ediff-merge-split-window-function 'split-window-vertically)
  (ediff-window-setup-function       'ediff-setup-windows-plain)
)

(use-package electric
  :config
  (electric-pair-mode     -1)
  (electric-indent-mode   -1)
)

(use-package files
  :demand t
  :functions emacs-tmp-dir
  :init
  (defconst emacs-tmp-dir (expand-file-name (format "emacs%d/" (user-uid)) temporary-file-directory))
  (setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t))
        backup-directory-alist         `((".*" . ,emacs-tmp-dir))
        auto-save-timeout              3
        auto-save-list-file-name       nil
        auto-save-interval             0
        auto-save-default              t
        auto-save-list-file-prefix     emacs-tmp-dir)
  :custom
  (backup-inhibited               t)
  (create-lockfiles               nil "Disable lockfiles .#filename")
  (delete-old-versions            t   "Don't ask to delete excess backup versions.")
  (make-backup-files              t)
  (version-control                t   "Use version numbers for backups.")
  (vc-make-backup-files           t   "Emacs never backs up versioned files")
  (kept-new-versions              5   "Number of newest versions to keep.")
  (kept-old-versions              0   "Number of oldest versions to keep.")
)

(use-package fringe
  :config
  (fringe-mode '(8 . 1))
)

(use-package imenu
  :demand t
  :custom
  (imenu-auto-rescan      t)
  (imenu-use-popup-menu   nil)
)

(use-package isearch
  :demand t
  :custom
  (search-highlight          t "Highlight search results")
  (query-replace-highlight   t)
  (auto-window-vscroll       nil)
)

(use-package ispell
  :custom t
  (add-to-list 'ispell-local-dictionary-alist
               '("english-hunspell"
                 "[[:alpha:]]"
                 "[^[:alpha:]]"
                 "[']"
                 t
                 ("-d" "en_US")
                 nil
                 utf-8))
  (add-to-list 'ispell-local-dictionary-alist
               '("deutsch-hunspell"
                 "[[:alpha:]]"
                 "[^[:alpha:]]"
                 "[']"
                 t
                 ("-d" "de_DE"); Dictionary file name
                 nil
                 utf-8))
  (add-to-list 'ispell-local-dictionary-alist
               '("russian"
                 "[АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюяіїєґ’A-Za-z]"
                 "[^АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюяіїєґ’A-Za-z]"
                 "[-']"
                 nil
                 ("-d" "ru_RU")
                 nil
                 utf-8))
  (add-to-list 'ispell-local-dictionary-alist
               '("ukrainian"
                 "[АБВГДЕЖЗИІЙКЛМНОПРСТУФХЦЧШЩЬЄЮЯабвгдежзиійклмнопрстуфхцчшщьєюяіїєґ’A-Za-z]"
                 "[^АБВГДЕЖЗИІЙКЛМНОПРСТУФХЦЧШЩЬЄЮЯабвгдежзиійклмнопрстуфхцчшщьєюяіїєґ’A-Za-z]"
                 "[-']"
                 nil
                 ("-d" "uk_UA")
                 nil
                 utf-8))
  (ispell-program-name "hunspell")
  ;(ispell-dictionary "russian")
  (ispell-really-aspell nil)
  (ispell-really-hunspell t)
  (ispell-encoding8-command t)
  (ispell-silently-savep t)
)

(use-package mule
  :demand t
  :config
  (prefer-coding-system        'utf-8)
  (set-keyboard-coding-system  'utf-8-unix)
  (set-language-environment    'UTF-8)
  (set-selection-coding-system 'utf-8)
  (set-terminal-coding-system  'utf-8)
  :custom
  (buffer-file-coding-system   'utf-8)
  (file-name-coding-system     'utf-8)
  (coding-system-for-read      'utf-8)
)

;; (use-package paren
;;   :init
;;   (show-paren-mode 2)
;;   :custom-face
;;   (show-paren-match ((t (:background "#1d2021" :foreground "#def" :weight extra-bold))))
;;   :custom
;;   (show-paren-delay 0.2)
;;   (show-paren-style 'parenthesis)
;; )

(use-package recentf
  :demand t
  :custom
  (recentf-max-saved-items 1500)
  (recentf-max-menu-items  150)
  (recentf-exclude         '("recentf" "elpa" "custom.el" ".cache"))
  (recentf-save-file       (concat user-emacs-directory ".my-recentf"))
  :config
  (recentf-mode t)
  :diminish nil)

(use-package saveplace
  :demand t
  :init
  (save-place-mode 1)
  :custom
  (save-place-ignore-files-regexp
   "\\(?:COMMIT_EDITMSG\\|hg-editor-[[:alnum:]]+\\.txt\\|elpa\\|svn-commit\\.tmp\\|bzr_log\\.[[:alnum:]]+\\)$")
  (save-place-file (concat user-emacs-directory ".my-saved-places"))
  (save-place-forget-unreadable-files t)
)

(use-package select
  :demand t
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

(use-package semantic
  :demand t
  :config
  (semantic-mode 1)
  :custom
  (semantic-which-function-use-color t)
)

(use-package sendmail
  :mode ("^/tmp/evo.*" . mail-mode)
)

(use-package sh-script
  :custom
  (sh-basic-offset 2)
  (sh-indentation  2)
)

(use-package shell
  :functions add-mode-line-dirtrack
  :config
  (defun add-mode-line-dirtrack ()
    (add-to-list 'mode-line-buffer-identification
                 '(:propertize (" " default-directory " ") face dired-directory)))
  :hook
  (shell-mode-hook . add-mode-line-dirtrack)
  :custom
  (shell-file-name "/bin/zsh" "Set zsh as default shell")
)

(use-package windmove
  :demand t
  :init
  (windmove-default-keybindings 'meta)
)

(use-package whitespace
  :demand t
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

(provide 'base-rcp)

;;; Commentary:
;; Configuration for base Emacs without packages

;;; base-rcp.el ends here
