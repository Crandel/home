;; Package manager:
;; Initialise package and add Melpa repository

(require 'package)

(setq my-packages
    '(
      el-get
      all-the-icons
      apib-mode
      avy
      company-mode
      company-flx
      company-restclient
      emmet-mode
      emacs-fish
      expand-region
      flycheck
      git-gutter
      helm
      helm-projectile
      helm-swoop
      json-mode
      key-chord
      know-your-http-well
      magit
      markdown-mode
      mo-git-blame
      multi-compile
      multiple-cursors
      neotree
      less-css-mode
      projectile
      restclient
      smartparens
      smart-mode-line
      undo-tree
      web-mode
      xclip
      yaml-mode
      yasnippet
      )
)
(when (executable-find "go")
    (add-to-list 'my-packages 'go-eldoc)
    (add-to-list 'my-packages 'go-mode)
    (add-to-list 'my-packages 'go-company)
    (add-to-list 'my-packages 'go-rename)
    (add-to-list 'my-packages 'go-scratch)
    (add-to-list 'my-packages 'flycheck-gometalinter)
)

(when (executable-find "node")
    (add-to-list 'my-packages 'js2-mode)
    (add-to-list 'my-packages 'js2-refactor)
    (add-to-list 'my-packages 'indium)
)

(when (executable-find "rg")
    (add-to-list 'my-packages 'helm-ag)
)

(when (executable-find "cargo")
    (add-to-list 'my-packages 'rust-mode)
    (add-to-list 'my-packages 'rust-racer)
    (add-to-list 'my-packages 'emacs-racer)
)

(when (executable-find "python")
    (add-to-list 'my-packages 'jedi-core)
    (add-to-list 'my-packages 'company-jedi)
    (add-to-list 'my-packages 'pip-requirements)
    (when (executable-find "autopep8")
      (add-to-list 'my-packages 'py-autopep8)
      )
    (add-to-list 'my-packages 'py-isort)
    (when (executable-find "virtualenv")
      (add-to-list 'my-packages 'auto-virtualenv))
)

(when (executable-find "livedown")
    (add-to-list 'my-packages 'livedown)
)

(when (executable-find "scala")
    (add-to-list 'my-packages 'ensime)
    (add-to-list 'my-packages 'sbt-mode)
    (add-to-list 'my-packages 'scala-mode)
)

(when (executable-find "javac")
    (add-to-list 'my-packages 'meghanada)
)

(when (executable-find "docker")
    (add-to-list 'my-packages 'dockerfile-mode)
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
  (package-install 'async)
  (package-install 'memoize)
  (message "require is")
  (require 'el-get)
  (el-get 'sync))

(add-to-list 'el-get-recipe-path "~/.emacs.d/settings/recipes")
(el-get 'sync my-packages)

(provide 'package_my)
