;;; early-init.el --- Pre init config
;;; Code:

;; Increase the GC threshold for faster startup
;; The default is 800 kilobytes.  Measured in bytes.
;; Set garbage collection threshold to 1GB.
(setq gc-cons-threshold #x40000000)

(setq-default user-emacs-directory (expand-file-name "~/.cache/emacs/")
      package-user-dir (expand-file-name "packages" user-emacs-directory)
      url-history-file (expand-file-name "url/history" user-emacs-directory)
      custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :noerror)

;; Native compilation settings
(when (featurep 'native-compile)
  ;; Set the right directory to store the native compilation cache
  (let ((path (expand-file-name "eln-cache/" user-emacs-directory)))
    (setq-default native-comp-eln-load-path       (list path)
                  native-compile-target-directory path)
    (when (fboundp 'startup-redirect-eln-cache)
      (startup-redirect-eln-cache path)))
  (setq-default native-comp-async-report-warnings-errors nil  ;; Silence compiler warnings as they can be pretty disruptive
                native-comp-deferred-compilation         t    ;; Make native compilation happens asynchronously
                package-native-compile                   t)   ;; Compile installed packages
  )

;; Themes
(add-to-list 'custom-theme-load-path (expand-file-name "themes/" (file-name-directory load-file-name)))
(load-theme 'tango-dark t)
(add-to-list 'load-path (expand-file-name "themes/" (file-name-directory load-file-name)))

(setq-default byte-compile-warnings     '(not obsolete)
      frame-resize-pixelwise    t  ;; Default frame configuration: full screen
      inhibit-startup-message   t
      load-prefer-newer         t  ;; Prefer loading newest compiled .el file
      package-enable-at-startup t
      warning-suppress-log-types '((comp) (bytecomp))
)

(when (member "Hack Nerd Font" (font-family-list))
  (set-frame-font "Hack Nerd Font-16" t t))
(set-fontset-font
   t
   'emoji
   (cond
    ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
    ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
    ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
    ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
    ((member "Symbola" (font-family-list)) "Symbola")))
(set-face-attribute 'font-lock-comment-face       nil :slant  'italic)
(set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
(set-face-attribute 'font-lock-variable-name-face nil :slant  'italic)
(set-window-scroll-bars (minibuffer-window) nil nil)
(set-default-coding-systems 'utf-8) ;; Set default coding system (especially for Windows)

(setq default-frame-alist
        '(
          (alpha 100 100)
          (cursor-color             . "#BE81F7")
          (font                     . "Hack Nerd Font-16")
          (fullscreen               . maximized)
          (tool-bar-lines           . 0)
          (inhibit-double-buffering . t)
          (vertical-scroll-bars     . right)))

;; Window configuration for special windows.
(add-to-list 'display-buffer-alist
             '("\\*Help\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)))

(add-to-list 'display-buffer-alist
             '("\\*Completions\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)
               (window-height . 10)))

;; Show dictionary definition on the left
(add-to-list 'display-buffer-alist
             '("^\\*Dictionary\\*"
               (display-buffer-in-side-window)
               (side . left)
               (window-width . 70)))

(blink-cursor-mode              1)
(column-number-mode             t)
(global-font-lock-mode          1)
(menu-bar-mode                  -1)
(scroll-bar-mode                1)
(tool-bar-mode                  -1)
(tooltip-mode                   -1)
(put 'downcase-region 'disabled nil)
(put 'upcase-region   'disabled nil)


;; Make the initial buffer load faster by setting its mode to fundamental-mode
(setq initial-major-mode 'fundamental-mode)

;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;;; early-init.el ends here
