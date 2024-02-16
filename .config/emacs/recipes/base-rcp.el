;;; base-rcp.el --- Emacs default configuration

;;; Code:
(eval-when-compile (require 'use-package))
(use-package emacs
  :demand t
  :config
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
  (defalias 'yes-or-no-p     'y-or-n-p)
  (with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill))
  :custom
  (ad-redefinition-action       'accept)
  (auto-window-vscroll          nil "Make scrolling less stuttered")
  (bidi-display-reordering      nil "Never reorder bidirectional text for display in the visual order.")
  (bidi-paragraph-direction     'left-to-right)
  (c-basic-offset               2)
  (completion-cycle-threshold   3)
  (completion-detailed          t)
  (display-time-24hr-format     t)
  (display-time-default-load-average nil)
  (display-time-mode            t)
  (enable-recursive-minibuffers t)
  (fast-but-imprecise-scrolling nil)
  (file-name-shadow-properties     '(invisible t intangible t face file-name-shadow field shadow)
                                   "Removing minibuffer 'default directory' prefix.")
  (file-name-shadow-tty-properties '(invisible t intangible t before-string "{" after-string "} " field shadow)
                                   "Removing minibuffer 'default directory' prefix in tty.")
  (frame-title-format              '((buffer-file-name "%f [%*] %I %P %l" "%b [%*] %I %P %l"))
                                   "Display the name of the current buffer in the title bar")
  (indent-line-function         'insert-tab "Indent settings")
  (indent-tabs-mode             nil "Indent settings")
  (indicate-empty-lines         nil "Scrolling settings")
  (initial-scratch-message      "")
  (java-basic-offset            2)
  (jit-lock-defer-time          0)
  (js-indent-level              2)
  (kill-do-not-save-duplicates  t)
  (lisp-body-indent             2)
  (max-mini-window-height       0.5)
  (maximum-scroll-margin        0.5 "Scrolling settings")
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (next-line-add-newlines     nil)
  (nxml-attribute-indent      2)
  (redisplay-dont-pause       t)
  (resize-mini-windows        t)
  (ring-bell-function         'ignore)
  (fast-but-imprecise-scrolling    t "Scrolling settings")
  (scroll-conservatively           most-positive-fixnum "Scrolling settings")
  (scroll-margin                   100 "Scrolling settings")
  (scroll-step                     1 "Scrolling settings")
  (scroll-preserve-screen-position t "Scrolling settings")
  (size-indication-mode       t)
  (sentence-end-double-space  nil)
  (split-height-threshold     nil "Minimum height for splitting windows vertically.")
  (split-width-threshold      0   "Minimum height for splitting windows horizontally.")
  (standart-indent            2)
  (tab-always-indent          nil)
  (tab-width                  2)
  (use-dialog-box             nil "Non-nil means mouse commands use dialog boxes to ask questions.")
  ;; (uniquify-buffer-name-style 'reverse)
  ;; (uniquify-min-dir-content   3)
  :bind
  ("M-h"      . backward-char)
  ("M-l"      . forward-char)
  ("C-k"      . backward-paragraph)
  ("C-j"      . forward-paragraph)
  ("C-v"      . yank)
  ("<Copy>"   . kill-ring-save)
  ("<Paste>"  . yank)
  ("C-y"      . scroll-up-command)
  ("RET"      . newline)
  ("M-RET"    . newline-and-indent)
  ("C-c d d"  . delete-other-windows)
  ("C-c d t"  . split-window-below)
  ("C-c d v"  . split-window-right)
  ("C-c d s"  . vd/save-all-buffers)
  ("C-c d b"  . kill-buffer)
  ("C-c d c"  . comment-dwim)
  ("C-c f d"  . vd/duplicate-line)
  ("C-c f l"  . vd/copy-line)
  ("C-c f r"  . (lambda()
                  (interactive)
                  (revert-bluffer t t)))
  ("C-c f s"  . sort-lines)
  ("C-c f w"  . delete-trailing-whitespace)
  ("C-c f c"  . vd/copy-word)
  ("C-c f x"  . vd/delete-line)
  ("C-c f f"  . find-file)
  ("C-b"      . list-buffers)
  ("C-c b"    . list-buffers)
  ("C-c q"    . vd/kill-emacs-with-save)
  ([remap list-buffers] . ibuffer-list-buffers) ;; C-x C-b by default
  ("C-x b"    . ibuffer)
  ("C-o"      . vd/open-next-line)
  ("C-O"      . vd/open-previous-line)
  ([M-S-down] . vd/move-line-down)
  ([M-S-up]   . vd/move-line-up)
  :hook
  (minibuffer-setup . cursor-intangible-mode)
  (prog-mode  . vd/highlight-todos)
  (after-save . executable-make-buffer-file-executable-if-script-p) ;; Make shebang (#!) file executable when saved
)

(use-package autorevert
  :defer 3
  :custom
  (global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode t)
)

(use-package compile
  :defer t
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

(use-package display-line-numbers
  :demand t
  :custom
  (display-line-numbers-type t)
  :config
  (global-display-line-numbers-mode t)
)

(use-package ediff
  :defer t
  :preface
  (defun vd/command-line-diff (switch)
      (let ((file1 (pop command-line-args-left))
            (file2 (pop command-line-args-left)))
        (ediff file1 file2)))
  (defun vd/command-line-dir-diff (switch)
      (let ((dir1 (pop command-line-args-left))
            (dir2 (pop command-line-args-left)))
        (ediff-directories dir1 dir2 ".*")))
  (defun vd/command-line-merge (switch)
      (let ((local    (pop command-line-args-left))
            (base     (pop command-line-args-left))
            (remote   (pop command-line-args-left))
            (merged   (pop command-line-args-left)))
        (ediff-merge-files-with-ancestor local remote base nil merged)))
  :init
  (add-to-list 'command-switch-alist '("diff" . vd/command-line-diff))
  (add-to-list 'command-switch-alist '("f-diff" . vd/command-line-dir-diff))
  (add-to-list 'command-switch-alist '("merge" . vd/command-line-merge))
  :custom
  (ediff-forward-word-function       'forward-char)
  (ediff-highlight-all-diffs         t)
  (ediff-merge-split-window-function 'split-window-horizontally)
  (ediff-split-window-function       'split-window-horizontally)
  (ediff-window-setup-function       'ediff-setup-windows-plain)
  :hook
  (ediff-startup      . ediff-next-difference)
)

(use-package electric
  :defer t
  :init
  (electric-pair-mode    1)
  (electric-indent-mode -1)
  :custom
  (electric-pair-pairs  '((34 . 34)
                          (8216 . 8217)
                          (8220 . 8221)
                          (123 . 125)
                          (40 . 41)
                          (60 . 62)))
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
)

(use-package emacs-lisp-mode
  :mode ("\\.el\\'" "\\.el.tmpl\\'")
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
  (confirm-kill-processes         nil)
  (create-lockfiles               nil "Disable lockfiles .#filename")
  (delete-old-versions            t   "Don't ask to delete excess backup versions.")
  (make-backup-files              nil)
  (version-control                t   "Use version numbers for backups.")
  (vc-make-backup-files           t   "Emacs never backs up versioned files")
  (kept-new-versions              5   "Number of newest versions to keep.")
  (kept-old-versions              0   "Number of oldest versions to keep.")
)

(use-package flyspell-mode
  :defer 1
)

(use-package fringe
  :demand t
  :config
  (fringe-mode '(8 . 1))
)

(use-package imenu
  :defer t
  :config
  (define-fringe-bitmap 'left-curly-arrow [16 48 112 240 240 112 48 16])
  (define-fringe-bitmap 'right-curly-arrow [8 12 14 15 15 14 12 8])
  :custom
  (imenu-auto-rescan      t)
  (imenu-use-popup-menu   nil)
)

(use-package ispell
  :defer t
  :custom
  (ispell-program-name        "hunspell")
  (ispell-local-dictionary    "en_US")
  (ispell-personal-dictionary "~/.cache/emacs/.ispell.dic")
  (ispell-really-aspell       nil)
  (ispell-really-hunspell     t)
  (ispell-encoding8-command   t)
  (ispell-silently-savep      t)
    (ispell-local-dictionary-alist
               '(("en_US"
                  "[[:alpha:]]"
                  "[^[:alpha:]]"
                  "[']"
                  t
                  ("-d" "en_US")
                  nil
                  utf-8)
                 ("de_DE"
                  "[[:alpha:]]"
                  "[^[:alpha:]]"
                  "[']"
                  t
                  ("-d" "de_DE"); Dictionary file name
                  nil
                  utf-8)
                 ("ru_RU"
                  "[АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюяіїєґ’A-Za-z]"
                  "[^АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюяіїєґ’A-Za-z]"
                  "[-']"
                  nil
                  ("-d" "ru_RU")
                  nil
                  utf-8)
                 ("uk_UA"
                  "[АБВГДЕЖЗИІЙКЛМНОПРСТУФХЦЧШЩЬЄЮЯабвгдежзиійклмнопрстуфхцчшщьєюяіїєґ’A-Za-z]"
                  "[^АБВГДЕЖЗИІЙКЛМНОПРСТУФХЦЧШЩЬЄЮЯабвгдежзиійклмнопрстуфхцчшщьєюяіїєґ’A-Za-z]"
                  "[-']"
                  nil
                  ("-d" "uk_UA")
                  nil
                  utf-8)))
)

(use-package make-mode
  :defer t
  :bind (
  (:map makefile-mode-map
        ("M-n" . nil)))
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

(use-package paren
  :defer 0.1
  :init
  (show-paren-mode 2)
  :custom-face
  (show-paren-match ((t (:background "#1d2021" :foreground "#def" :weight extra-bold))))
  :custom
  (show-paren-delay 0.2)
  (show-paren-style 'parenthesis)
)

(use-package recentf
  :defer 0.5
  :custom
  (recentf-max-saved-items 1500)
  (recentf-max-menu-items  150)
  (recentf-exclude         '("recentf" "elpa" "eln-cache" "semanticdb" "transient" "bookmarks"))
  (recentf-save-file       (concat user-emacs-directory ".my-recentf"))
  :config
  (recentf-mode t)
  (run-at-time nil 60 'recentf-save-list)
  (defun no-msg (function)
    "Prevent FUNCTION from showing messages in echo area."
    (let ((inhibit-message  t))
      (funcall function)))
  (advice-add 'recentf-save-list :around 'no-msg)
  :diminish nil)

(use-package savehist
  :demand t
  :config
  (savehist-mode)
)

(use-package saveplace
  :defer 3
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
  :init
  (setq-default wl-copy-process nil)
  (when (string-prefix-p "wayland" (getenv "WAYLAND_DISPLAY"))
    (defun wl-copy-handler (text)
      (setq wl-copy-process (make-process :name "wl-copy"
                                          :buffer nil
                                          :command '("wl-copy" "-f" "-n")
                                          :connection-type 'pipe))
      (process-send-string wl-copy-process text)
      (process-send-eof wl-copy-process))
    (defun wl-paste-handler ()
      (if (and wl-copy-process (process-live-p wl-copy-process))
          nil ; should return nil if we're the current paste owner
        (shell-command-to-string "wl-paste -n | tr -d \r")))
    (setq interprogram-cut-function 'wl-copy-handler
          interprogram-paste-function 'wl-paste-handler))
)

(use-package sendmail
  :defer t
  :mode ("^/tmp/evo.*" . mail-mode)
)

(use-package sh-script
  :defer t
  :custom
  (sh-basic-offset 2)
  (sh-indentation  2)
)

(use-package shell
  :defer t
  :functions add-mode-line-dirtrack
  :config
  (defun add-mode-line-dirtrack ()
    (add-to-list 'mode-line-buffer-identification
                 '(:propertize (" " default-directory " ") face dired-directory)))
  :hook
  (shell-mode . add-mode-line-dirtrack)
  :custom
  (shell-file-name "/usr/bin/zsh" "Set zsh as default shell")
)

(use-package subword
  :config
  (global-subword-mode t)
)

(use-package tramp
  :disabled)

(use-package windmove
  :defer t
  :ensure nil
  :init
  (windmove-default-keybindings 'meta)
  :bind
  ("C-c d h" . windmove-left)
  ("C-c d l" . windmove-right)
  ("C-c d j" . windmove-down)
  ("C-c d k" . windmove-up)
)

(use-package winner-mode
  :ensure nil
  :defer 1
  :init
  (winner-mode)
)

(use-package whitespace
 :demand t
 :init
 (global-whitespace-mode t)
 :custom-face
 (whitespace-empty ((t (:foreground "sienna"))))
 (whitespace-hspace ((t (:background "grey24" :foreground "MistyRose4"))))
 (whitespace-indentation ((t (:background unspecified :foreground "DarkOrchid4"))))
 (whitespace-newline ((t (:foreground "dark green" :weight normal))))
 (whitespace-space ((t (:foreground "DarkOrchid4"))))
 (whitespace-space-after-tab ((t (:foreground "firebrick"))))
 (whitespace-space-before-tab ((t (:foreground "firebrick"))))
 (whitespace-tab ((t (:foreground "magenta"))))
 (whitespace-trailing ((t (:foreground "yellow" :weight bold))))
 (hl-line ((t (:background unspecified))))
 :custom
 (whitespace-style '(face trailing spaces lines-char empty indentation::tab
                     indentation::space tabs newline space-mark tab-mark newline-mark))
 (whitespace-global-modes '(not magit-diff-mode))
 (whitespace-line-column 130)
 ;; all numbers are Unicode codepoint in decimal. ⁖ (insert-char 182 1)
 (whitespace-display-mappings
  '(
    (space-mark 32 [183] [46]) ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
    (newline-mark 10 [8617 10]) ; 10 LINE FEED
    (lines-char 10 [8617 10]) ; 10 LINE FEED
    (tab-mark 9 [8594 9] [183 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
    ))
)

(use-package xref
  :defer t
  :bind
  ("M-,"   . xref-find-references)
  ("C-,"   . xref-go-back)
  ("C-M-," . xref-go-forward)
)

(provide 'base-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;; Configuration for base Emacs without packages
;;; base-rcp.el ends here
