(add-to-list 'load-path (expand-file-name "settings" user-emacs-directory))
(require 'dark-mint-theme)
(require 'my_scratch)
(require 'my_package)
(require 'my_hooks)
(require 'my_keybindings)

;; Custom section
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview-common ((t (:inherit company-preview :foreground "brightcyan"))))
 '(company-preview-search ((t (:inherit company-preview :background "black"))))
 '(company-scrollbar-bg ((t (:background "color-16"))))
 '(company-scrollbar-fg ((t (:background "brightyellow"))))
 '(company-template-field ((t (:background "black" :foreground "yellow"))))
 '(company-tooltip ((t (:background "black" :foreground "brightyellow"))))
 '(company-tooltip-selection ((t (:background "color-108"))))
 '(powerline-active1 ((t (:inherit mode-line :background "color-235"))))
 '(powerline-active2 ((t (:inherit mode-line :background "color-220"))))
 '(whitespace-empty ((t (:foreground "color-166"))))
 '(whitespace-indentation ((t (:foreground "color-241"))))
 '(whitespace-line ((t (:underline (:color foreground-color :style wave)))))
 '(whitespace-newline ((t (:foreground "color-130" :weight normal))))
 '(whitespace-space ((t (:foreground "color-236"))))
 '(whitespace-space-after-tab ((t (:foreground "firebrick"))))
 '(whitespace-space-before-tab ((t (:foreground "firebrick"))))
 '(whitespace-tab ((t (:foreground "magenta"))))
 '(whitespace-trailing ((t (:foreground "yellow" :weight bold)))))
