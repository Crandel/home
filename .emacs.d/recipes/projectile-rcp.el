(:name projectile
       :description "Project navigation and management library for Emacs."
       :type github
       :pkgname "bbatsov/projectile"
       :depends (dash pkg-info ag)
       :load ("projectile.el")
       :post-init (progn
                    (projectile-mode +1)
                    (add-to-list 'projectile-globally-ignored-directories "*bloop")
                    (add-to-list 'projectile-globally-ignored-directories "*metals")
                    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
                    ))
