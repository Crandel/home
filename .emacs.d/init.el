(add-to-list 'load-path (expand-file-name "settings" user-emacs-directory))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'gruvbox t)
(require 'scratch_my)
(require 'keybindings_my)
(require 'package_my)
(require 'hooks_my)

;; Paren face
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#def")
(set-face-attribute  'show-paren-match nil :weight 'extra-bold)

;; Custom section
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (ripgrep reverse-im flymake jsonrpc memoize async)))
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-buffer-modified ((t (:inherit font-lock-comment-face :foreground "coral"))))
 '(magit-diff-added ((t (:background "dark slate gray" :foreground "chocolate"))))
 '(magit-diff-added-highlight ((t (:background "dark olive green" :foreground "gold"))))
 '(magit-diff-removed ((t (:background "red" :foreground "#ffdddd"))))
 '(magit-diff-removed-highlight ((t (:background "dark red" :foreground "navajo white"))))
 '(mode-line-buffer-id-inactive ((t (:inherit mode-line-buffer-id :foreground "dark goldenrod"))))
 '(sml/read-only ((t (:inherit sml/not-modified :foreground "deep sky blue"))))
 '(whitespace-empty ((t (:foreground "sienna"))))
 '(whitespace-hspace ((t (:background "grey24" :foreground "MistyRose4"))))
 '(whitespace-indentation ((t (:foreground "DarkOrchid4"))))
 '(whitespace-newline ((t (:foreground "dark green" :weight normal))))
 '(whitespace-space ((t (:foreground "DarkOrchid4"))))
 '(whitespace-space-after-tab ((t (:foreground "firebrick"))))
 '(whitespace-space-before-tab ((t (:foreground "firebrick"))))
 '(whitespace-tab ((t (:foreground "magenta"))))
 '(whitespace-trailing ((t (:foreground "yellow" :weight bold)))))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
