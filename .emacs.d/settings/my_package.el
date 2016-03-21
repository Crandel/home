;; Package manager:
;; Initialise package and add Melpa repository

(require 'package)

(setq my-packages
    '(el-get
      emmet-mode
      ergoemacs-mode
      multiple-cursors
      py-autopep8
      smartparens
      yasnippet))

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

(el-get-bundle elpa:dark-mint-theme
    (load-theme 'dark-mint t))

(el-get-bundle magit
    (global-set-key (kbd "C-x C-z") 'magit-status))

(el-get-bundle! marcwebbie/auto-virtualenv
    :depends (cl-lib pyvenv s)
    (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv))

(el-get-bundle! elpa:py-isort
    :build ("sudo pip install -U isort"))

(el-get-bundle projectile
    (add-hook 'python-mode-hook 'projectile-mode)
    (add-hook 'javascript-mode-hook 'projectile-mode))

(el-get-bundle company-mode
    (add-hook 'after-init-hook 'global-company-mode))

(el-get-bundle company-flx
    (with-eval-after-load 'company
        (company-flx-mode +1)))

(el-get-bundle company-jedi
    :depends (cl-lib company-mode jedi-core)
    (with-eval-after-load 'company
        (add-hook 'python-mode-hook
            (add-to-list 'company-backends 'company-jedi ))))

(el-get-bundle flycheck
    (add-hook 'after-init-hook #'global-flycheck-mode))

(provide 'my_package)
