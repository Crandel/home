;; Package manager:
;; Initialise package and add Melpa repository

(require 'package)

(setq my-packages
    '(
        el-get
        auto-virtualenv
        company-mode
        company-flx
        company-jedi
        dark-mint-theme
        emmet-mode
        ;ergoemacs-mode
        flycheck
        jedi-core
        multiple-cursors
        py-autopep8
        py-isort
        projectile
        smartparens
        yasnippet
      )
)

;; for gnu repository
(setq package-check-signature nil)

(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
    (package-refresh-contents)
    (package-install 'el-get)
    (message "require is")
    (require 'el-get)
    (el-get 'sync))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync my-packages)

(provide 'my_package)
