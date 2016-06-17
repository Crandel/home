;; Package manager:
;; Initialise package and add Melpa repository

(require 'package)

(setq my-packages
    '(
        el-get
        auto-virtualenv
        avy
        company-mode
        company-flx
        company-jedi
        emmet-mode
        emacs-fish
        expand-region
        flycheck
        flycheck-gometalinter
        git-gutter
        go-mode
        go-company
        helm
        helm-ag
        helm-ls-git
        helm-projectile
        helm-swoop
        jedi-core
        json-mode
        key-chord
        magit
        markdown-mode
        mo-git-blame
        multiple-cursors
        neotree
        less-css-mode
        livedown
        pip-requirements
        powerline
        projectile
        py-autopep8
        py-isort
        restclient
        smartparens
        undo-tree
        web-mode
        xclip
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

(add-to-list 'el-get-recipe-path "~/.emacs.d/settings/recipes")
(el-get 'sync my-packages)

(provide 'package_my)
