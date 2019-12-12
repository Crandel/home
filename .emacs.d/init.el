(package-initialize)

(add-to-list 'load-path (expand-file-name "settings" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))
(require 'dark-mint-theme)
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
 '(compilation-disable-input t)
 '(compilation-window-height 10)
 '(custom-safe-themes
   (quote
    ("b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(magit-log-arguments
   (quote
    ("--graph" "--color" "--decorate" "--show-signature" "--follow" "-n256")))
 '(magit-log-margin-show-committer-date t)
 '(magit-log-remove-graph-args (quote ("--follow" "--grep" "-G" "-S" "-L")))
 '(neo-vc-state-char-alist
   (quote
    ((up-to-date . 32)
     (edited . 69)
     (added . 43)
     (removed . 45)
     (missing . 33)
     (needs-merge . 77)
     (conflict . 33)
     (unlocked-changes . 33)
     (needs-update . 85)
     (ignored . 38)
     (user . 85)
     (unregistered . 40)
     (nil . 40))))
 '(neo-window-fixed-size nil)
 '(neo-window-width 35)
 '(package-selected-packages (quote (reverse-im flymake jsonrpc memoize async)))
 '(powerline-default-separator (quote arrow))
 '(powerline-default-separator-dir (quote (right . left)))
 '(semantic-which-function-use-color t)
 '(sml/pos-id-separator "> ")
 '(sml/pos-minor-modes-separator "|")
 '(sml/pre-id-separator "<")
 '(sml/pre-minor-modes-separator " ")
 '(sml/pre-modes-separator " ")
 '(sr-show-hidden-files t)
 '(sr-tree-explosion-ratio 5)
 '(sr-tree-isearch-always-sticky t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview-common ((t (:inherit company-preview :foreground "brightcyan"))))
 '(company-preview-search ((t (:inherit company-preview :background "black"))))
 '(company-scrollbar-bg ((t (:background "dark green"))))
 '(company-scrollbar-fg ((t (:background "red"))))
 '(company-template-field ((t (:background "black" :foreground "chartreuse"))))
 '(company-tooltip ((t (:background "black" :foreground "yellow green"))))
 '(company-tooltip-search ((t (:background "dark khaki" :underline "blue"))))
 '(company-tooltip-selection ((t (:background "honeydew" :foreground "dark green"))))
 '(helm-buffer-modified ((t (:inherit font-lock-comment-face :foreground "coral"))))
 '(magit-diff-added ((t (:background "dark slate gray" :foreground "chocolate"))))
 '(magit-diff-added-highlight ((t (:background "dark olive green" :foreground "gold"))))
 '(magit-diff-removed ((t (:background "red" :foreground "#ffdddd"))))
 '(magit-diff-removed-highlight ((t (:background "dark red" :foreground "navajo white"))))
 '(mode-line-buffer-id-inactive ((t (:inherit mode-line-buffer-id :foreground "dark goldenrod"))))
 '(neo-vc-edited-face ((t (:foreground "magenta"))))
 '(neo-vc-ignored-face ((t (:foreground "dim gray" :underline nil))))
 '(neo-vc-missing-face ((t (:foreground "red"))))
 '(neo-vc-removed-face ((t (:foreground "saddle brown"))))
 '(neo-vc-unlocked-changes-face ((t (:foreground "dark turquoise"))))
 '(neo-vc-unregistered-face ((t (:foreground "yellow"))) t)
 '(neo-vc-up-to-date-face ((t (:foreground "dark olive green"))))
 '(neo-vc-user-face ((t (:foreground "red" :slant italic))))
 '(powerline-active1 ((t (:inherit mode-line :background "saddle brown" :foreground "green yellow"))))
 '(powerline-active2 ((t (:inherit mode-line :background "green4" :foreground "khaki"))))
 '(sml/read-only ((t (:inherit sml/not-modified :foreground "deep sky blue"))))
 '(sr-active-path-face ((t (:foreground "yellow" :weight bold :height 120))))
 '(sr-editing-path-face ((t (:foreground "yellow" :underline "red" :weight bold :height 120))))
 '(sr-highlight-path-face ((t (:foreground "#ace6ac" :underline "magenta" :weight bold :height 120))))
 '(sr-mirror-path-face ((t (:foreground "yellow" :underline "sienna" :weight bold :height 120))))
 '(sr-passive-path-face ((t (:foreground "lightgray" :underline "white" :weight bold :height 120))))
 '(whitespace-empty ((t (:foreground "sienna"))))
 '(whitespace-hspace ((t (:background "grey24" :foreground "MistyRose4"))))
 '(whitespace-indentation ((t (:foreground "dim gray"))))
 '(whitespace-line ((t (:underline (:color foreground-color :style wave)))))
 '(whitespace-newline ((t (:foreground "dark green" :weight normal))))
 '(whitespace-space ((t (:foreground "DarkOrchid4"))))
 '(whitespace-space-after-tab ((t (:foreground "firebrick"))))
 '(whitespace-space-before-tab ((t (:foreground "firebrick"))))
 '(whitespace-tab ((t (:foreground "magenta"))))
 '(whitespace-trailing ((t (:foreground "yellow" :weight bold)))))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
