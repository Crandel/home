;;; init.el --- Main init
;;; Code:
;; Set garbage collection threshold to 1GB.
(setq gc-cons-threshold #x40000000)

(add-to-list 'custom-theme-load-path (expand-file-name "themes/" (file-name-directory load-file-name)))
(load-theme 'gruvbox t)
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :noerror)

(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))

(eval-when-compile
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (setq gnutls-algorithm-priority  "NORMAL:-VERS-TLS1.3" ;; bug fix for gnu
        package-enable-at-startup nil
        package-archive-priorities '(("melpa"        . 200)
                                     ("elpa"         . 100)
                                     ("org"          . 75)
                                     ("gnu"          . 50)))  ;; Higher values are searched first.
;(setq package-check-signature nil) ;; for gnu repository
  (package-initialize t)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (put 'use-package 'lisp-indent-function 1)

  (use-package use-package-core
    :custom
    ; (use-package-verbose t)
    (use-package-minimum-reported-time 0.005)
    (use-package-enable-imenu-support t))
)
(add-to-list 'load-path (expand-file-name "recipes/" (file-name-directory load-file-name)))
;; (debug-watch 'company-backends)
(require 'functions_my)

(require 'base-rcp) ; emacs default settings
(require 'use-package-chords-rcp) ; provide :chords for use-package
;; (require 'quelpa-rcp) ; install from different sourses

;; (require 'fnhh-rcp)
(require 'avy-rcp)
(require 'all-the-icons-rcp)
;;(require 'bazel-rcp)
(require 'company-rcp)
;;(require 'corfu-rcp)
(require 'consult-rcp)
(require 'dired-subtree-rcp)
(require 'embark-rcp)
(require 'emmet-mode-rcp)
;; (require 'expand-region-rcp)
(require 'evil-rcp)
(require 'flycheck-rcp)
(require 'git-rcp)
;;(require 'helm-rcp)
(require 'helpful-rcp)
(require 'highlight-indentation-rcp)
(require 'i3wm-config-rcp)
(require 'json-mode-rcp)
(require 'lsp-mode-rcp)
(require 'lua-rcp)
(require 'marginalia-rcp)
(require 'markdown-mode-rcp)
(require 'multi-compile-rcp)
;(require 'multiple-cursors-rcp)
;; (require 'mini-frame-rcp)
(require 'orderless-rcp)
(require 'org-mode-rcp)
(require 'pkgbuild-rcp)
(require 'projectile-rcp)
(require 'rainbow-rcp)
(require 'restclient-rcp)
;; (require 'selectrum-rcp)
(require 'smart-mode-line-rcp)
;; (require 'smartparens-rcp)
;; (require 'telega-rcp)
(require 'treemacs-rcp)
(require 'tree-sitter-rcp)
;; (require 'undo-tree-rcp)
(require 'vertico-rcp)
(require 'vimrc-mode-rcp)
(require 'web-mode-rcp)
(require 'which-key-rcp)
(require 'yasnippet-rcp)
(require 'ytdl-rcp)
(require 'ztree-rcp)
(require 'k8s-mode-rcp)
(require 'rust-rcp)
(require 'docker-rcp)
(require 'go-rcp)
(require 'python-rcp)
(require 'ripgrep-rcp)
(require 'terraform-rcp)
(require 'editorconfig-rcp)
(require 'org-jira-rcp)


(add-function :after after-focus-change-function
  (defun me/garbage-collect-maybe ()
    (unless (frame-focus-state)
      (garbage-collect))))

;;; Commentary:
;; Main init file
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; init.el ends here
