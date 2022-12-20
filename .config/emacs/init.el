;;; init.el --- Main init
;;; Code:
(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))

(eval-when-compile
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (setq gnutls-algorithm-priority  "NORMAL:-VERS-TLS1.3" ;; bug fix for gnu
        package-enable-at-startup nil
        package-archive-priorities '(("melpa"        . 200)
                                     ("elpa"         . 100)
                                     ("org"          . 75)
                                     ("nongnu"       . 65)
                                     ("gnu"          . 50)))  ;; Higher values are searched first.
;(setq package-check-signature nil) ;; for gnu repository
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
  (use-package use-package-ensure-system-package
    :ensure t)
)
(add-to-list 'load-path (expand-file-name "recipes/" (file-name-directory load-file-name)))
;; (debug-watch 'completion-at-point-functions)
(require 'functions_my)

(require 'base-rcp) ; emacs default settings
(require 'use-package-chords-rcp) ; provide :chords for use-package
;; (require 'quelpa-rcp) ; install from different sourses

(require 'avy-rcp)
(require 'all-the-icons-rcp)
(require 'corfu-rcp)
(require 'consult-rcp)
(require 'dired-rcp)
(require 'embark-rcp)
(require 'emmet-mode-rcp)
(require 'evil-rcp)
(require 'flycheck-rcp)
(require 'git-rcp)
(require 'helpful-rcp)
(require 'highlight-indentation-rcp)
(require 'hydra-rcp)
(require 'json-mode-rcp)
(require 'lsp-mode-rcp)
(require 'lua-rcp)
(require 'marginalia-rcp)
(require 'markdown-mode-rcp)
(require 'multi-compile-rcp)
(require 'orderless-rcp)
(require 'org-mode-rcp)
(require 'pkgbuild-rcp)
(require 'projectile-rcp)
(require 'rainbow-rcp)
(require 'restclient-rcp)
(require 'smart-mode-line-rcp)
(require 'svg-tag-mode-rcp)
(require 'tempel-rcp)
(require 'tree-sitter-rcp)
(require 'turbo-log-rcp)
(require 'vertico-rcp)
(require 'vimrc-mode-rcp)
(require 'which-key-rcp)
;; (require 'yasnippet-rcp)
(require 'ytdl-rcp)
(require 'ztree-rcp)

(require 'web-mode-rcp)
(require 'i3wm-config-rcp)

(require 'rust-rcp)
(require 'docker-rcp)
(require 'go-rcp)
(require 'python-rcp)
(require 'ripgrep-rcp)


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
