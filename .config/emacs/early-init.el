;;; early-init.el --- Pre init config
;;; Code:

;; Increase the GC threshold for faster startup
;; The default is 800 kilobytes.  Measured in bytes.
;; Set garbage collection threshold to 1GB.
(setq gc-cons-threshold #x40000000)

(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      package-user-dir (expand-file-name "packages" user-emacs-directory)
      url-history-file (expand-file-name "url/history" user-emacs-directory)
      custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :noerror)

;; Native compilation settings
(when (fboundp 'native-comp-available-p)
  ;; Set the right directory to store the native compilation cache
  (let ((path (expand-file-name "eln-cache/" user-emacs-directory)))
    (setq native-comp-eln-load-path (list path)
          native-compile-target-directory path))
  (setq native-comp-async-report-warnings-errors nil ;; Silence compiler warnings as they can be pretty disruptive
        native-comp-deferred-compilation         t)  ;; Make native compilation happens asynchronously
  )

;; Themes
(add-to-list 'custom-theme-load-path (expand-file-name "themes/" (file-name-directory load-file-name)))
(load-theme 'gruvbox t)

;; Prefer loading newest compiled .el file
(setq load-prefer-newer       noninteractive
      inhibit-startup-message t)

(set-frame-font   "Hack Nerd Font-16" "Font settings")
(set-fontset-font "fontset-default" 'unicode "Source Code Pro")
(set-fontset-font t nil (font-spec :size 16 :name "Noto Color Emoji"))
(set-window-scroll-bars (minibuffer-window) nil nil)
(set-default-coding-systems 'utf-8) ;; Set default coding system (especially for Windows)

(setq default-frame-alist
        '(
          (alpha 100 100)
          (cursor-type . 'hbar)
          (cursor-color . "#BE81F7")
          (font . "Hack Nerd Font-16")
          (tool-bar-lines . 0)
          (vertical-scroll-bars . right)))

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
