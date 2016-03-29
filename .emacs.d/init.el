(add-to-list 'load-path (expand-file-name "settings" user-emacs-directory))
(require 'my_scratch)
(require 'my_package)
(require 'my_hooks)
(require 'my_keybindings)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-show-hidden-files t)
 '(neo-theme (quote arrow)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-vc-added-face ((t (:foreground "brightyellow")))))
