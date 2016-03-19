;; Package manager:
;; Initialise package and add Melpa repository

(require 'cl)
(require 'package)

(setq cfg:packages
  '(auto-complete
    company-mode ;; Complete All
    company-jedi
    company-flx
    emmet-mode
    ergoemacs-mode
    flycheck ;; Syntax check on fly
    multiple-cursors
    py-autopep8
    ;py-isort
    projectile ;; Удобный менеджер проектов
    smartparens
    yasnippet
    ;auto-virtualenv ;; Auto virtualenv activates virtualenv automatically when called.
    ))

;; for gnu repository
(setq package-check-signature nil)

;(defun cfg:install-packages ()
;    (let ((pkgs (remove-if #'package-installed-p cfg-var:packages)))
;        (when pkgs
;            (message "%s" "Emacs refresh packages database...")
;            (package-refresh-contents)
;            (message "%s" " done.")
;            (dolist (p cfg:packages)
;                (package-install p)))))

(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
    (package-refresh-contents)
    (package-install 'el-get)
    (require 'el-get))
(el-get 'sync)
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync cfg:packages)

;(cfg:install-packages)

;; Auto-virtualenv
;(require 'auto-virtualenv)
;(add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
;; Projectile virtualenv
(add-hook 'projectile-after-switch-project-hook 'auto-virtualenv-set-virtualenv)
;; Company
(add-hook 'after-init-hook 'global-company-mode)
;; company fix
(with-eval-after-load 'company
    (company-flx-mode +1))
;; company jedi
(defun my/python-mode-hook ()
      (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)
;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(global-flycheck-mode)
;; Smartparent
(require 'smartparens-config)
(smartparens-global-mode 1)

(provide 'my_package)
