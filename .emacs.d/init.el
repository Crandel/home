(add-to-list 'load-path (expand-file-name "settings" user-emacs-directory))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'gruvbox t)

(require 'keybindings_my)

(require 'package)
;; for gnu repository
;(setq package-check-signature nil)
;; bug fix for gnu
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;; Higher values are searched first.
(setq package-archive-priorities
      '(
        ("melpa"        . 200)
        ("org"          . 100)
        ("elpa"         . 75)
        ("gnu"          . 50)))
(customize-set-variable 'package-enable-at-startup nil)

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(put 'use-package 'lisp-indent-function 1)

(use-package use-package-core
  :custom
  ; (use-package-verbose t)
  (use-package-minimum-reported-time 0.005)
  (use-package-enable-imenu-support t))

(add-to-list 'load-path "~/.emacs.d/recipes")


;; require for backup
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d/" (user-uid)) temporary-file-directory))

(require 'base-rcp) ; emacs default settings
(require 'bind-key) ; if you use any :bind variant
(require 'bind-chord) ; if you use any :chords variant
(require 'use-package-chords-rcp)
(require 'quelpa-rcp) ; install from different sourses

(require 'avy-rcp)
(require 'company-rcp)
(require 'company-box-rcp)
(require 'fnhh-rcp)
(require 'helm-rcp)
(require 'lsp-mode-rcp)
(require 'magit-rcp)
(require 'multiple-cursors-rcp)
(require 'smart-mode-line-rcp)
(require 'smartparens-rcp)
(require 'treemacs-rcp)
(require 'undo-tree-rcp)
(require 'which-key-rcp)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
