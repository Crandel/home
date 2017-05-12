;; Font settings
(set-default-font "Hack 12")
;; The full name of the user logged in
;; Full mailing address of user
(setq-default user-full-name	 "crandel"
							user-mail-adress "cradlemann@gmail.com")

;; Set bash as default shell
(setq shell-file-name						"/bin/bash"
			explicit-shell-file-name	"/bin/bash")
(blink-cursor-mode 0)
;; (setenv "GOPATH"
;;	 (concat
;;				 (getenv "HOME")
;;				 "/go"))
;; (setenv "PATH"
;;	 (concat
;;		(getenv "PATH") ":"
;;		(getenv "GOPATH") ":"
;;		(getenv "GOPATH") "/bin"
;;	 )
;; )
;; (add-to-list 'exec-path (concat (getenv "GOPATH") "/bin"))

;; Inhibit startup/splash screen
(setq inhibit-splash-screen		t
			inhibit-startup-message t)

;; Cursor
(setq-default cursor-type 'bar)
(set-cursor-color "#BE81F7")

;; Imenu
(require 'imenu)
(setq imenu-auto-rescan			 t
			imenu-use-popup-menu	 nil)
(semantic-mode 1)

;; SavePlace
(save-place-mode 1)
(setq save-place-file											"~/.emacs.d/saved-places"
			save-place-forget-unreadable-files		t)

;; Electric-modes settings
(electric-pair-mode		-1)
(electric-indent-mode	-1)
;; Delete selection
(delete-selection-mode t)

;; Disable GUI components
(tooltip-mode		-1)
(menu-bar-mode		-1)
(tool-bar-mode		-1)
(menu-bar-mode		-1)
(scroll-bar-mode	-1)
(setq use-dialog-box				nil
			redisplay-dont-pause	t
			ring-bell-function		'ignore)

;; Display the name of the current buffer in the title bar
(setq frame-title-format "%b")

;; Disable backup/autosave files
(setq backup-inhibited					t
			make-backup-files				nil
			auto-save-default				nil
			auto-save-list-file-name nil)

;; Coding-system settings
(set-language-environment							'UTF-8)
(setq buffer-file-coding-system				'utf-8
			file-name-coding-system					'utf-8)
(setq-default coding-system-for-read		'utf-8)
(set-selection-coding-system						'utf-8)
(set-keyboard-coding-system						'utf-8-unix)
(set-terminal-coding-system						'utf-8)
(prefer-coding-system									'utf-8)

;; Linum plugin
(require 'linum)
(line-number-mode			t)
(global-linum-mode		t)
(column-number-mode		t)
(setq linum-format		" %d")

;; Fringe settings
(fringe-mode '(8 . 0))
(setq-default indicate-buffer-boundaries 'left)

;; Display file size/time in mode-line
(setq display-time-24hr-format	t)
(display-time-mode							t)
(size-indication-mode						t)
(defun add-mode-line-dirtrack ()
	(add-to-list 'mode-line-buffer-identification
							'(:propertize (" " default-directory " ") face dired-directory)))
(add-hook 'shell-mode-hook 'add-mode-line-dirtrack)

;; Indent settings
(setq-default indent-tabs-mode			t
							tab-width							2
							tab-always-indent			nil
							c-basic-offset				2
							sh-basic-offset				2
							scala-basic-offset		2
							standart-indent				2
							lisp-body-indent			2
							indent-line-function	'insert-tab)

;; Scrolling settings
(setq scroll-step						1
			scroll-margin					10
			scroll-conservatively	10000)

;; Short messages
(defalias 'yes-or-no-p 'y-or-n-p)

;; Clipboard settings
(setq x-select-enable-clipboard t)

(setq next-line-add-newlines nil)

;; Highlight search resaults
(setq search-highlight					t
			query-replace-highlight		t
			auto-window-vscroll				nil)
(setq bidi-display-reordering		nil)
;;; Whitespace
(require 'whitespace)
(autoload 'global-whitespace-mode	"whitespace" "Toggle whitespace visualization." t)
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

(setq split-height-threshold nil
			split-width-threshold	0)

(if (equal nil (equal major-mode 'org-mode))
		(windmove-default-keybindings 'meta))

(recentf-mode 1)
(setq recentf-max-menu-items			550
			recentf-max-saved-items		550)

;; Show paren
(setq show-paren-delay 0
			show-paren-style 'expression)
(show-paren-mode 2)

(setq ns-pop-up-frames					nil
			ad-redefinition-action		'accept)

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

;(defadvice read-passwd (around my-read-passwd act)
;	(let ((local-function-key-map nil))
;		ad-do-it))
;(reverse-input-method 'russian-typewriter)

(setq max-mini-window-height		0.4
			compilation-always-kill		t)


(provide 'scratch_my)
