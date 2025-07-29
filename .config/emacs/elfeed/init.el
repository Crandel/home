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
(load-theme 'gruvbox t)
(require 'functions_my)

(require 'base-rcp) ; emacs default settings

;; info packages
(require 'nerd-icons-rcp)
(require 'mini-echo-rcp)

;; Rest packages
(require 'consult-rcp)
(require 'dired-rcp)
(require 'elfeed-rcp)
(require 'embark-rcp)
(require 'helpful-rcp)
(require 'highlight-rcp)
(require 'hydra-rcp)
(require 'marginalia-rcp)
(require 'orderless-rcp)
(require 'org-mode-rcp)
(require 'rainbow-rcp)
(require 'vertico-rcp)
(require 'undo-rcp)
(require 'meow-rcp)
(require 'final-steps-rcp)

;;; Commentary:
;; Main init file
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; init.el ends here
