;;; base-rcp.el --- Emacs default configuration

;;; Code:
(eval-when-compile (require 'use-package))
(use-package emacs
  :demand t
  :config
  (defalias 'yes-or-no-p     'y-or-n-p)
  (with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill))
  :custom
  (ad-redefinition-action            'accept)
  (bidi-display-reordering           nil "Never reorder bidirectional text for display in the visual order.")
  (bidi-paragraph-direction          'left-to-right)
  (c-basic-offset                    2)
  (cursor-in-non-selected-windows    nil)
  (display-time-24hr-format          t)
  (display-time-default-load-average nil)
  (display-time-mode                 t)
  (enable-recursive-minibuffers      nil)
  (file-name-shadow-properties     '(invisible t intangible t face file-name-shadow field shadow)
                                   "Removing minibuffer 'default directory' prefix.")
  (file-name-shadow-tty-properties '(invisible t intangible t before-string "{" after-string "} " field shadow)
                                   "Removing minibuffer 'default directory' prefix in tty.")
  (frame-title-format              '((buffer-file-name "%f [%*] %I %P %l" "%b [%*] %I %P %l"))
                                   "Display the name of the current buffer in the title bar")
  (highlight-nonselected-windows   nil)
  (help-window-select              t)
  (indent-line-function            'insert-tab "Indent settings")
  (indent-tabs-mode                nil "Indent settings")
  (indicate-empty-lines            nil "Scrolling settings")
  (initial-scratch-message         "")
  (java-basic-offset               2)
  (jit-lock-defer-time             0)
  (js-indent-level                 2)
  (kill-do-not-save-duplicates     t)
  (lisp-body-indent                2)
  (max-mini-window-height          0.5)
  (maximum-scroll-margin           0.5 "Scrolling settings")
  (minibuffer-prompt-properties    '(read-only t cursor-intangible t face minibuffer-prompt))
  (next-line-add-newlines          nil)
  (nxml-attribute-indent           2)
  (read-extended-command-predicate #'command-completion-default-include-p) ;; Hide commands in M-x which do not apply to the current mode.
  (redisplay-dont-pause            t)
  (resize-mini-windows             'grow-only)
  (resize-mini-frames              t)
  (ring-bell-function              'ignore)
  (size-indication-mode            t)
  (sentence-end-double-space       nil)
  ;; (split-height-threshold          nil "Minimum height for splitting windows vertically.")
  ;; (split-width-threshold           0   "Minimum height for splitting windows horizontally.")
  (standart-indent                 2)
  (tab-always-indent               'complete)
  (tab-width                       2)
  (use-dialog-box                  nil "Non-nil means mouse commands use dialog boxes to ask questions.")
  (word-wrap                       t)
  ;; (uniquify-buffer-name-style 'reverse)
  ;; (uniquify-min-dir-content   3)
  :bind
  ("M-h"      . backward-char)
  ("M-l"      . forward-char)
  ("C-k"      . backward-paragraph)
  ("C-j"      . forward-paragraph)
  ("M-j"      . scroll-up)
  ("M-k"      . scroll-down)
  ("C-v"      . yank)
  ("C-h F"    . describe-face)
  ("<Copy>"   . kill-ring-save)
  ("<Paste>"  . yank)
  ("C-y"      . scroll-up-command)
  ("RET"      . newline)
  ("M-RET"    . reindent-then-newline-and-indent)
  ([remap delete-char] . delete-forward-char)
  ("C-c d c"  . comment-dwim)
  ("C-c f c"  . vd/copy-word)
  ("C-c f d"  . delete-trailing-whitespace)
  ("C-c f f"  . find-file)
  ("C-c f l"  . vd/copy-line)
  ("C-c f o"  . duplicate-dwim)
  ("C-c f r"  . vd/revert-buffer)
  ("C-c f s"  . sort-lines)
  ("C-c f u"  . upcase-region)
  ("C-c f w"  . save-file)
  ("C-c f x"  . vd/delete-line)
  ("C-c f j"  . hippie-expand)
  ("C-r"      . hippie-expand)
  ("C-b"      . list-buffers)
  ("C-c b"    . list-buffers)
  ("C-c q"    . vd/kill-emacs-with-save)
  ([remap list-buffers] . ibuffer-list-buffers) ;; C-x C-b by default
  ("C-x b"    . ibuffer)
  ("C-o"      . vd/open-next-line)
  ("C-S-o"    . vd/open-previous-line)
  ([M-S-down] . vd/move-line-down)
  ([M-S-up]   . vd/move-line-up)
  ("C-z"      . nil) ; suspend-frame
  ("C-x C-z"  . nil) ; suspend-frame
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p) ;; Make shebang (#!) file executable when saved
)

(use-package autorevert
  :defer 3
  :custom
  (global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode t)
)

(use-package auth-source
  :defer 2
  :custom
  (auth-source-cache-expiry nil)
  (auth-sources '("~/.authinfo"))
  :config
)

(use-package compile
  :defer t
  :custom
  (compilation-always-kill              t)
  (compilation-auto-jump-to-first-error t)
  (compilation-disable-input            t)
  (compilation-max-output-line-length   nil)
  (compilation-scroll-output            t)
  (compilation-window-height            10)
  :hook
  (compilation-filter . ansi-color-compilation-filter)
)

(use-package completion-preview
  :disabled
  ;; :init
  ;; (global-completion-preview-mode t)
  :custom
  (completion-preview-commands
   '(self-insert-command insert-char delete-backward-char
                         backward-delete-char-untabify
                         analyze-text-conversion))
  (completion-preview-minimum-symbol-length 1)
  (completion-preview-idle-delay 0.1)
  :bind (
  :map completion-preview-active-mode-map
       ([right] . completion-preview-next-candidate)
       ("C-j"   . completion-preview-next-candidate)
       ([left]  . completion-preview-prev-candidate)
       ("C-k"   . completion-preview-prev-candidate)
       ("C-t"   . vd/tab-indent-or-complete)
  )
)

(use-package css-ts-mode
  :mode ("\\.css\\'" "\\.css.tmpl\\'")
)

(use-package delsel
  :demand t
  :config
  (delete-selection-mode t)
)

(use-package display-line-numbers
  :demand t
  :custom
  (display-line-numbers-type  t)
  (display-line-numbers-widen t)
  (display-line-numbers-width 3)
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
  (ediff-keep-variants               nil)
  (ediff-merge-split-window-function 'split-window-horizontally)
  (ediff-split-window-function       'split-window-horizontally)
  (ediff-window-setup-function       'ediff-setup-windows-plain)
  :hook
  (ediff-startup      . ediff-next-difference)
)

(use-package electric
  :defer t
  :init
  (electric-pair-mode   -1)
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
  (enable-local-variables         :all)
  (make-backup-files              nil)
  (version-control                t   "Use version numbers for backups.")
  (vc-make-backup-files           t   "Emacs never backs up versioned files")
  (kept-new-versions              5   "Number of newest versions to keep.")
  (kept-old-versions              0   "Number of oldest versions to keep.")
)

(use-package flyspell
  :defer 1
  :custom-face
  (flyspell-incorrect ((t (:underline (:color "dark violet" :style wave :position nil)))))
  :bind (
  :map flyspell-mode-map
  ("C-." . nil)
  ("C-," . nil)
  )
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
)

(use-package fringe
  :demand t
  :config
  (fringe-mode '(8 . 1))
)

(use-package ibuffer
  :defer t
  :custom
  (ibuffer-deletion-char 128298)
  (ibuffer-locked-char 128477)
  (ibuffer-marked-char 127919)
  (ibuffer-modified-char 9935)
  (ibuffer-read-only-char 128683)
)

(use-package icomplete
  :disabled
  :custom
  (icomplete-compute-delay               0)
  (icomplete-delay-completions-threshold 0)
  (icomplete-in-buffer                   t)
  (icomplete-max-delay-chars             0)
  (icomplete-prospects-height            15)
  (icomplete-separator                   " . ")
  (icomplete-show-matches-on-no-input    t)
  (icomplete-tidy-shadowed-file-names    t)
  (icomplete-with-completion-tables      t)
  ;; :init
  ;; (fido-vertical-mode t)
  ;; (advice-add 'icomplete-fido-ret :after #'vd/minibuffer-history)
  :bind (
  :map icomplete-fido-mode-map
  ("C-j"   . icomplete-forward-completions)
  ("C-k"   . icomplete-backward-completions)
  ("TAB"   . icomplete-forward-completions)
  )
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

(use-package jsonrpc
  :config
  (fset #'jsonrpc--log-event #'ignore) ;; speed up lsp output.
)

(use-package make-mode
  :defer t
  :bind (
  (:map makefile-mode-map
        ("M-n" . nil)))
)

(use-package man
  :defer t
  :custom-face
  (Man-overstrike ((t (:inherit 'bold :foreground "orange red"))))
  (Man-underline ((t (:inherit 'underline :foreground "forest green"))))
)

(use-package minibuffer
  :preface
  (defun vd/complete-styles ()
    (setq-local completion-styles '(basic initials substring partial-completion)))
  :custom
  (completion-auto-help                            nil)
  (completion-category-defaults                    nil)
  (completion-cdabbrev-prompt-flag                 t)
  (completion-cycle-threshold                      3)
  (completion-in-region-mode                       t)
  (completion-ignore-case                          t)
  (completion-on-separator-character               t)
  (completion-pcm-complete-word-inserts-delimiters t)
  (completion-styles                               '(basic initials substring))
  (completion-category-overrides '(
   (buffer   (styles . (initials substring partial-completion)))
   (file     (styles . (initials substring partial-completion)))
   (command  (styles . (initials substring partial-completion)))
   (symbol   (styles . (initials partial-completion)))
   (variable (styles . (initials partial-completion)))
   ))
  (completions-detailed                            t)
  (dynamic-completion-mode                         t)
  :init
  (advice-add 'completion-at-point
              :after #'minibuffer-hide-completions)
  (setq minibuffer-frame-alist '(
            (name . "minibuf")
            (menu-bar-lines . 0)
            (vertical-scroll-bars . nil)
            (auto-raise . t)
            (sticky . t)
            (left . 0)
            (top . -1)
            (height . 1)
            (internal-border-width . 0)
            (minibuffer . only)))
  :hook
  (minibuffer-setup           . cursor-intangible-mode)
  (icomplete-minibuffer-setup . vd/complete-styles)
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

(use-package outline
  :hook
  (prog-mode . outline-minor-mode)
)

(use-package paren
  :defer 0.1
  :init
  (show-paren-mode 2)
  :custom-face
  (show-paren-match ((t (:background "#1d2021" :foreground "#def" :weight extra-bold))))
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-context-when-offscreen  t)
  (show-paren-delay                   0.2)
  (show-paren-style                   'mixed)
)

(use-package pixel-scroll
  :defer 0.1
  :custom
  (fast-but-imprecise-scrolling    t "Scrolling settings")
  (scroll-conservatively           most-positive-fixnum "Scrolling settings")
  (scroll-margin                   100 "Scrolling settings")
  (scroll-step                     1 "Scrolling settings")
  (scroll-preserve-screen-position t "Scrolling settings")
  :hook
  (prog-mode . pixel-scroll-precision-mode)
)

(use-package proced
  :defer t
  :custom
  (proced-enable-color-flag t)
  (proced-tree-flag t)
  (proced-auto-update-flag 'visible)
  (proced-auto-update-interval 1)
  (proced-descent t)
  (proced-filter 'user) ;; We can change interactively with `s'
  :config
  (add-hook 'proced-mode-hook
            (lambda ()
              (proced-toggle-auto-update 1))))

(use-package prog-mode
  :hook
  (prog-mode . (lambda()
                 (vd/highlight-todos)
                 ;; (setq prettify-symbols-alist
                 ;;       '(("lambda" . ?λ)
                 ;;         ("->"     . ?→)
                 ;;         ("->>"    . ?↠)
                 ;;         ("=>"     . ?⇒)
                 ;;         ("map"    . ?↦)
                 ;;         ("/="     . ?≠)
                 ;;         ("!="     . ?≠)
                 ;;         ("=="     . ?≡)
                 ;;         ("<="     . ?≤)
                 ;;         (">="     . ?≥)
                 ;;         ("<<"     . ?≪)
                 ;;         (">>"     . ?≫)
                 ;;         ("<=<"    . ?↢)
                 ;;         (">=>"    . ?↣)
                 ;;         ("sqrt"   . ?√)
                 ;;         ("..."    . ?…)))
                  (prettify-symbols-mode nil)))
  :bind
  (:map prog-mode-map
    ("C-c TAB" . prog-indent-sexp)
    )
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
  :custom
  (history-delete-duplicates t)
  (history-length            3000)
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

(use-package smerge-mode
  :custom
  (smerge-command-prefix "C-c t s")
)

(use-package subword
  :config
  (global-subword-mode t)
)

(use-package tramp
  :disabled)

(use-package webjump
  :defer t
  :ensure nil
  :bind ("C-x /" . webjump)
  :custom
  (webjump-sites
   '(("DuckDuckGo" . [simple-query "duckduckgo.com" "duckduckgo.com/?q=" ""])
     ("Google"     . [simple-query "google.com" "google.com/search?q=" ""])
     ("YouTube"    . [simple-query "youtube.com/feed/subscriptions" "youtube.com/rnesults?search_query=" ""])
     ("Wikipedia"  . [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])
     ("Archwiki"   . [simple-query "wiki.archlinux.org" "wiki.archlinux.org/index.php?search=" ""])
     ("ChatGPT"    . [simple-query "chatgpt.com" "chatgpt.com/?q=" ""])))
)

(use-package which-key
  :defer 1
  :commands (which-key--show-keymap which-key--hide-popup-ignore-command)
  :custom
  (which-key-separator              "  ")
  (which-key-prefix-prefix          "->")
  (which-key-max-description-length 40)
  (which-key-show-transient-maps    t)
  (which-key-popup-type 'minibuffer)
  :hook
  (after-init . which-key-mode)
)

(use-package window
  :defer t
  :ensure nil
  :bind
  ("C-c d d" . delete-other-windows)
  ("C-c d s" . vd/save-all-buffers)
  ("C-c d b" . kill-buffer)
  ("C-c d p" . previous-buffer)
  ("C-c d t" . split-window-below)
  ("C-c d v" . split-window-right)
  ("C-c d H" . shrink-window-horizontally)
  ("C-c d J" . enlarge-window)
  ("C-c d K" . shrink-window)
  ("C-c d L" . enlarge-window-horizontally)
)

(use-package windmove
  :defer t
  :ensure nil
  :init
  (windmove-default-keybindings 'meta)
  :bind
  ("C-c <left>"  . windmove-left)
  ("C-c <right>" . windmove-right)
  ("C-c <down>"  . windmove-down)
  ("C-c <up>"    . windmove-up)
  ("C-c d h"     . windmove-left)
  ("C-c d l"     . windmove-right)
  ("C-c d j"     . windmove-down)
  ("C-c d k"     . windmove-up)
  ("C-c d x h"   . windmove-delete-left)
  ("C-c d x l"   . windmove-delete-right)
  ("C-c d x j"   . windmove-delete-down)
  ("C-c d x k"   . windmove-delete-up)
)

(use-package winner-mode
  :ensure nil
  :defer 1
  :custom
  (winner-dont-bind-my-keys t)
  :init
  (winner-mode)
  :bind
  ("C-c d u" . winner-undo)
  ("C-c d r" . winner-redo)
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
 (whitespace-line ((t (:underline (:color "violet" :style dots :position nil)))))
 (whitespace-tab ((t (:foreground "magenta"))))
 (whitespace-trailing ((t (:foreground "yellow" :weight bold))))
 (hl-line ((t (:background unspecified))))
 :custom
 (whitespace-style '(face trailing tabs spaces lines-tail newline missing-newline-at-eof empty
                     indentation::tab space-mark tab-mark newline-mark))
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
  :config
  (add-hook 'xref-after-jump-hook #'delete-other-windows)
)

(provide 'base-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;; Configuration for base Emacs without packages
;;; base-rcp.el ends here
