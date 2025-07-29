;;; init.el --- Main init
;;; Code:
(eval-when-compile
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
  (setq gnutls-algorithm-priority  "NORMAL:-VERS-TLS1.3" ;; bug fix for gnu
        package-enable-at-startup  nil
        package-archive-priorities '(("melpa"        . 200)
                                    ("elpa"         . 100)
                                    ("nongnu"       . 65)
                                    ("gnu"          . 50)))  ;; Higher values are searched first.
  (require 'use-package)
  (put 'use-package 'lisp-indent-function 1)

  (use-package use-package-core
    :custom
    ; (use-package-verbose t)
    (use-package-minimum-reported-time 0.005)
    (use-package-enable-imenu-support t))
)

;; Doom-theme load
(use-package doom-themes
  :custom
  (doom-themes-enable-bold   t)    ; if nil, bold is universally disabled
  (doom-themes-enable-italic t)
  ;; (doom-everforest-background  "hard")
  ;; (doom-everforest-palette     "material")
  :config
  (load-theme 'doom-gruvbox t)
  (use-package doom-themes-ext-org
    :config
    (doom-themes-org-config)
  )
)

(require 'functions_my)

(require 'base-rcp) ; emacs default settings

;; info packages
(require 'nerd-icons-rcp)
(require 'mini-echo-rcp)

;; Rest packages
(require 'aidermacs-rcp)
(require 'c-rcp)
(require 'chezmoi-rcp)
(require 'consult-rcp)
;; (require 'combobulate-rcp)
(require 'compilation-rcp)
(require 'corfu-rcp)
(require 'csv-mode-rcp)
(require 'dape-rcp)
(require 'dired-rcp)
(require 'editorconfig-rcp)
(require 'eglot-rcp)
(require 'ellama-rcp)
(require 'embark-rcp)
(require 'emmet-mode-rcp)
(require 'flymake-rcp)
; (require 'flycheck-rcp)
(require 'git-rcp)
(require 'grep-rcp)
(require 'gptel-rcp)
(require 'helpful-rcp)
(require 'highlight-rcp)
(require 'hydra-rcp)
(require 'journalctl-rcp)
(require 'js-rcp)
(require 'json-rcp)
(require 'kotlin-rcp)
(require 'lua-rcp)
(require 'marginalia-rcp)
(require 'markdown-mode-rcp)
;; (require 'monkeytype-rcp)
;; (require 'minuet-rcp)
(require 'mail-rcp)
(require 'mpc-rcp)
(require 'multi-compile-rcp)
(require 'nov-rcp)
(require 'orderless-rcp)
(require 'org-mode-rcp)
(require 'pkgbuild-rcp)
(require 'projectile-rcp)
(require 'rainbow-rcp)
(require 'selected-rcp)
(require 'smartparens-rcp)
(require 'sql-rcp)
(require 'tempel-rcp)
(require 'turbo-log-rcp)
(require 'http-rcp)
(require 'vertico-rcp)
(require 'vimrc-mode-rcp)
(require 'undo-rcp)
(require 'ytdl-rcp)
(require 'ztree-rcp)

(require 'web-mode-rcp)
(require 'i3wm-config-rcp)
(require 'tree-sitter-rcp)

(require 'rust-rcp)
(require 'docker-rcp)
(require 'go-rcp)
(require 'python-rcp)
;; meow should be the last one to collect all keybindings.
(require 'meow-rcp)
(require 'final-steps-rcp)
;(debug-on-variable-change 'completion-styles)

;;; Commentary:
;; Main init file
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; init.el ends here
