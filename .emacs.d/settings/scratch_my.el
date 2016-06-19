;; Font settings
(set-default-font "Hack 14")
;; The full name of the user logged in
(setq-default user-full-name "crandel")
;; Full mailing address of user
(setq-default user-mail-adress "cradlemann@gmail.com")

;; Set bash as default shell
(setq shell-file-name "/bin/bash")
(setenv "GOPATH"
  (concat
        (getenv "HOME")
        "/go"))
(setenv "PATH"
  (concat
   (getenv "PATH") ":"
   (getenv "GOPATH") ":"
   (getenv "GOPATH") "/bin"
  )
)
(add-to-list 'exec-path (concat (getenv "GOPATH") "/bin"))

;; Inhibit startup/splash screen
(setq inhibit-splash-screen   t)
(setq inhibit-startup-message t) ;; экран приветствия можно вызвать комбинацией C-h C-a

;; Cursor
(setq-default cursor-type 'bar)
(set-cursor-color "#BE81F7")

;; Imenu
(require 'imenu)
(setq imenu-auto-rescan      t) ;; автоматически обновлять список функций в буфере
(setq imenu-use-popup-menu nil) ;; диалоги Imenu только в минибуфере
(semantic-mode 1)

;; Ido
;(require 'ido)
;(ido-mode t)
;(icomplete-mode t)
;(ido-everywhere t)
;(setq ido-virtual-buffers t)
;(setq ido-enable-flex-matching t)
;(setq ido-all-frames t)
;(setq ido-auto-merge-delay-time 0)
;(setq ido-enable-flex-matching t)

;; SavePlace
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")
(setq save-place-forget-unreadable-files t)

;; Electric-modes settings
(electric-pair-mode   -1) ;; автозакрытие {},[],() с переводом курсора внутрь скобок
(electric-indent-mode -1) ;; отключить индентацию  electric-indent-mod'ом (default in Emacs-24.4)
;; Delete selection
(delete-selection-mode t)

;; Disable GUI components
(tooltip-mode      -1)
(menu-bar-mode     -1) ;; отключаем графическое меню
(tool-bar-mode     -1) ;; отключаем tool-bar
(menu-bar-mode     -1) ;; отключаем графическое меню
(scroll-bar-mode   -1) ;; отключаем полосу прокрутки
(setq use-dialog-box     nil) ;; никаких графических диалогов и окон - все через минибуфер
(setq redisplay-dont-pause t)  ;; лучшая отрисовка буфера
(setq ring-bell-function 'ignore) ;; отключить звуковой сигнал

;; Display the name of the current buffer in the title bar
(setq frame-title-format "%b")

;; Disable backup/autosave files
(setq backup-inhibited           t)
(setq make-backup-files        nil)
(setq auto-save-default        nil)
(setq auto-save-list-file-name nil) ;; я так привык... хотите включить - замените nil на t

;; Coding-system settings
(set-language-environment 'UTF-8)
(setq buffer-file-coding-system 'utf-8)
(setq-default coding-system-for-read    'utf-8)
(setq file-name-coding-system           'utf-8)
(set-selection-coding-system            'utf-8)
(set-keyboard-coding-system        'utf-8-unix)
(set-terminal-coding-system             'utf-8)
(prefer-coding-system                   'utf-8)

;; Linum plugin
(require 'linum) ;; вызвать Linum
(line-number-mode   t) ;; показать номер строки в mode-line
(global-linum-mode  t) ;; показывать номера строк во всех буферах
(column-number-mode t) ;; показать номер столбца в mode-line
(setq linum-format " %d") ;; задаем формат нумерации строк

;; Fringe settings
(fringe-mode '(8 . 0)) ;; органичиталь текста только слева
(setq-default indicate-buffer-boundaries 'left) ;; индикация только слева

;; Display file size/time in mode-line
(setq display-time-24hr-format t) ;; 24-часовой временной формат в mode-line
(display-time-mode             t) ;; показывать часы в mode-line
(size-indication-mode          t) ;; размер файла в %-ах
(defun add-mode-line-dirtrack ()
    (add-to-list 'mode-line-buffer-identification 
       '(:propertize (" " default-directory " ") face dired-directory)))
(add-hook 'shell-mode-hook 'add-mode-line-dirtrack)

;; Indent settings
(setq-default indent-tabs-mode nil) ;; отключить возможность ставить отступы TAB'ом
(setq-default tab-width          4) ;; ширина табуляции - 4 пробельных символа
(setq tab-width                  4) ;; ширина табуляции - 4 пробельных символа
(setq-default tab-always-indent nil) ;; make tab key call indent command or insert tab character, depending on cursor position
(setq-default c-basic-offset     4)
(setq-default standart-indent    4) ;; стандартная ширина отступа - 4 пробельных символа
(setq-default lisp-body-indent   4) ;; сдвигать Lisp-выражения на 4 пробельных символа
(setq indent-line-function  'insert-tab)

;; Scrolling settings
(setq scroll-step               1) ;; вверх-вниз по 1 строке
(setq scroll-margin            10) ;; сдвигать буфер верх/вниз когда курсор в 10 шагах от верхней/нижней границы
(setq scroll-conservatively 10000)

;; Short messages
(defalias 'yes-or-no-p 'y-or-n-p)

;; Clipboard settings
(setq x-select-enable-clipboard t)

(setq next-line-add-newlines nil) ;; не добавлять новую строку в конец при смещении курсора  стрелками

;; Highlight search resaults
(setq search-highlight        t
      query-replace-highlight t
      auto-window-vscroll     nil)

;;; Whitespace
(require 'whitespace)
(autoload 'global-whitespace-mode  "whitespace" "Toggle whitespace visualization." t)
(setq whitespace-style
 '(face trailing spaces lines-tail empty indentation::tab indentation::space tabs newline space-mark tab-mark newline-mark))
(global-whitespace-mode 1)
(setq whitespace-display-mappings
  ;; all numbers are Unicode codepoint in decimal. ⁖ (insert-char 182 1)
  '(
    (space-mark 32 [183] [46]) ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
    (newline-mark 10 [8617 10]) ; 10 LINE FEED
    (lines-tail 10 [8617 10]) ; 10 LINE FEED
    (tab-mark 9 [8594 9] [183 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
  )
  whitespace-line-column 130)

(setq split-height-threshold nil)
(setq split-width-threshold 0)

(if (equal nil (equal major-mode 'org-mode))
    (windmove-default-keybindings 'meta))

(recentf-mode 1)
(setq recentf-max-menu-items 150)
(setq recentf-max-saved-items 100)

;; Show paren
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(show-paren-mode 2)

(setq ns-pop-up-frames nil)
(setq ad-redefinition-action 'accept)

(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1))

;; Russian
(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

(defadvice read-passwd (around my-read-passwd act)
  (let ((local-function-key-map nil))
    ad-do-it))

 (reverse-input-method 'russian-typewriter)

(provide 'scratch_my)
