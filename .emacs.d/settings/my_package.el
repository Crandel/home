;; Package manager:
;; Initialise package and add Melpa repository

(require 'cl)
(require 'package)

(setq my-packages
    '(el-get
      auto-complete
      auto-virtualenv
      cl-lib
      company-flx
      company-jedi
      company-mode
      dash
      emmet-mode
      epl
      ergoemacs-mode
      flx
      flycheck
      fuzzy
      let-alist
      multiple-cursors
      package
      pkg-info
      popup
      py-autopep8
      py-isort
      pyvenv
      smartparens
      yasnippet))

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
(unless (featurep 'el-get)
    (message "Hello"))

(unless (require 'el-get nil t)
    (package-refresh-contents)
    (package-install 'el-get)
    (message "require is")
    ;(el-get 'sync)
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync my-packages)

;(cfg:install-packages)

;; Auto-virtualenv
;(require 'auto-virtualenv)
;(add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
;; Projectile virtualenv
;(add-hook 'projectile-after-switch-project-hook 'auto-virtualenv-set-virtualenv)
;;; Company
;(add-hook 'after-init-hook 'global-company-mode)
;;; company fix
;(with-eval-after-load 'company
;    (company-flx-mode +1))
;;; company jedi
;(defun my/python-mode-hook ()
;      (add-to-list 'company-backends 'company-jedi))
;(add-hook 'python-mode-hook 'my/python-mode-hook)
;;; Flycheck
;(add-hook 'after-init-hook #'global-flycheck-mode)
;(global-flycheck-mode)
;;; Smartparent
;(require 'smartparens-config)
;(smartparens-global-mode 1)

(provide 'my_package)
