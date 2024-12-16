;;; early-init.el --- Pre init config
;;; Code:

;; Increase the GC threshold for faster startup
;; The default is 800 kilobytes.  Measured in bytes.
;; Set garbage collection threshold to 1GB.
(setq gc-cons-threshold (if (display-graphic-p) 400000000 100000000))

(eval-and-compile
  (defun vd/garbage-collect-maybe ()
    (unless (frame-focus-state)
      (garbage-collect))))

(add-function :after after-focus-change-function 'vd/garbage-collect-maybe)

;; Default locations is in system cache directory.
(setq-default vd/emacs-config-directory (file-name-directory load-file-name)
              user-emacs-directory   (expand-file-name "emacs/" (getenv "XDG_CACHE_HOME"))
              package-user-dir       (expand-file-name "packages/" user-emacs-directory)
              url-history-file       (expand-file-name "url/history" user-emacs-directory)
              custom-file            (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :noerror)

;; Native compilation settings
(when (featurep 'native-compile)
  ;; Set the right directory to store the native compilation cache
  (let ((path (expand-file-name "eln-cache/" user-emacs-directory)))
    (setq-default native-compile-target-directory path
                  native-comp-eln-load-path       (list path)
                  native-comp-async-report-warnings-errors nil  ;; Silence compiler warnings
                  native-comp-deferred-compilation         t    ;; Make native compilation happens asynchronously
                  native-comp-speed                        2
                  package-native-compile                   t)   ;; Compile installed packages
    (when (fboundp 'startup-redirect-eln-cache)
      (startup-redirect-eln-cache path))
    )
)

;; Themes
(add-to-list 'custom-theme-load-path (expand-file-name "themes/" vd/emacs-config-directory))
(load-theme  'tango-dark t)
(add-to-list 'load-path (expand-file-name "themes/"  vd/emacs-config-directory))
(add-to-list 'load-path (expand-file-name "recipes/" vd/emacs-config-directory))

(setq-default auto-window-vscroll             nil
              byte-compile-warnings           '(not obsolete)
              frame-inhibit-implied-resize    t
              frame-resize-pixelwise          t  ;; Default frame configuration: full screen
              inhibit-default-init            t
              inhibit-startup-message         t
              load-prefer-newer               noninteractive  ;; Prefer loading newest compiled .el file
              package-enable-at-startup       t
              process-adaptive-read-buffering nil ;; speadup emacs
              site-run-file                nil
              warning-suppress-log-types   '((comp) (bytecomp))
)

(when (member "Hack Nerd Font" (font-family-list))
  (set-frame-font "Hack Nerd Font-16" t t))
(set-fontset-font
   t
   'emoji
   (cond
    ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
    ((member "Noto Color Emoji"  (font-family-list)) "Noto Color Emoji")
    ((member "Noto Emoji"        (font-family-list)) "Noto Emoji")
    ((member "Segoe UI Emoji"    (font-family-list)) "Segoe UI Emoji")
    ((member "Symbola"           (font-family-list)) "Symbola")))
(set-face-attribute 'font-lock-comment-face       nil :slant  'italic)
(set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
(set-face-attribute 'font-lock-variable-name-face nil :slant  'italic)

(set-window-scroll-bars (minibuffer-window) nil nil)

(set-default-coding-systems 'utf-8) ;; Set default coding system (especially for Windows)

(modify-all-frames-parameters '((width                    . 100)
                                (height                   . 100)
                                (alpha-background         . 99)
                                (cursor-color             . "#BE81F7")
                                (font                     . "Hack Nerd Font-16")
                                (fullscreen               . maximized)
                                (inhibit-double-buffering . t)
                                (internal-border-width    . 1)
                                (ns-appearance            . dark)
                                (tool-bar-lines           . 0)
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
;; End:
;;; early-init.el ends here
