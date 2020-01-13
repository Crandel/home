;; Font settings
(set-default-font "Hack 12")
;; The full name of the user logged in
;; Full mailing address of user
(setq-default user-full-name   "crandel"
              user-mail-adress "cradlemann@gmail.com")

;; Set zsh as default shell
(setq shell-file-name           "/bin/zsh"
      explicit-shell-file-name  "/bin/zsh")

;; Inhibit startup/splash screen
(setq inhibit-splash-screen   t
      inhibit-startup-message t)

;; Imenu
(require 'imenu)
(setq imenu-auto-rescan      t
      imenu-use-popup-menu   nil)
(semantic-mode 1)

;; SavePlace
(save-place-mode 1)
(setq save-place-file                       "~/.emacs.d/saved-places"
      save-place-forget-unreadable-files    t)

;; Electric-modes settings
(electric-pair-mode     -1)
(electric-indent-mode   -1)
;; Delete selection
(delete-selection-mode t)

;; Emacs GUI settings
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

(tooltip-mode     -1)
(menu-bar-mode    -1)
(setq use-dialog-box        nil
      redisplay-dont-pause  t
      ring-bell-function    'ignore)

;; Display the name of the current buffer in the title bar
(setq frame-title-format "%b")

;; Disable backup/autosave files
(setq backup-inhibited          t
      make-backup-files         nil
      auto-save-default         nil
      auto-save-list-file-name  nil)

;; Coding-system settings
(set-language-environment               'UTF-8)
(setq buffer-file-coding-system         'utf-8
      file-name-coding-system           'utf-8)
(setq-default coding-system-for-read    'utf-8)
(set-selection-coding-system            'utf-8)
(set-keyboard-coding-system             'utf-8-unix)
(set-terminal-coding-system             'utf-8)
(prefer-coding-system                   'utf-8)

(setq-default display-line-numbers t)

;; Display file size/time in mode-line
(setq display-time-24hr-format  t)
(display-time-mode              t)
(size-indication-mode           t)
(defun add-mode-line-dirtrack ()
  (add-to-list 'mode-line-buffer-identification
              '(:propertize (" " default-directory " ") face dired-directory)))
(add-hook 'shell-mode-hook 'add-mode-line-dirtrack)

;; Indent settings
(setq-default indent-tabs-mode      nil
              tab-width             4
              tab-always-indent     nil
              c-basic-offset        2
              sh-basic-offset       2
              sh-indentation        2
              scala-basic-offset    2
              java-basic-offset     4
              standart-indent       4
              lisp-body-indent      2
              js-indent-level       2
              indent-line-function  'insert-tab)

;; Scrolling settings
(setq scroll-step             1
      scroll-margin           10
      scroll-conservatively   10000)

;; Short messages
(defalias 'yes-or-no-p 'y-or-n-p)

;; Clipboard settings
(setq x-select-enable-clipboard t)

(setq next-line-add-newlines nil)

;; Highlight search resalts
(setq search-highlight            t
      query-replace-highlight     t
      auto-window-vscroll         nil
      bidi-display-reordering     nil)

;; Whitespace
(require 'whitespace)
(autoload 'global-whitespace-mode   "whitespace" "Toggle whitespace visualization." t)
(setq whitespace-style
      '(face trailing spaces lines-tail empty indentation::tab indentation::space tabs newline space-mark tab-mark newline-mark))
(global-whitespace-mode 1)
(setq whitespace-global-modes '(not magit-diff-mode))
(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal. ⁖ (insert-char 182 1)
      '(
        (space-mark 32 [183] [46]) ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
        (newline-mark 10 [8617 10]) ; 10 LINE FEED
        (lines-tail 10 [8617 10]) ; 10 LINE FEED
        (tab-mark 9 [8594 9] [183 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
        )
      whitespace-line-column 130)

(setq split-height-threshold  nil
      split-width-threshold   0)

(if (equal nil (equal major-mode 'org-mode))
    (windmove-default-keybindings 'meta))

(recentf-mode 1)
(setq recentf-max-menu-items      150
      recentf-max-saved-items     550)

;; Show paren
(setq show-paren-delay 0
      show-paren-style 'expression)
(show-paren-mode 2)

(setq ns-pop-up-frames          nil
      ad-redefinition-action    'accept)

(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1))

;; Russian
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

(defadvice read-passwd (around my-read-passwd act)
  (let ((local-function-key-map nil))
    ad-do-it))

(setq max-mini-window-height      0.5
      compilation-always-kill     t)

(provide 'scratch_my)
