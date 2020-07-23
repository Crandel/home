(:name dired-hacks
       :description "Collection of useful dired additions."
       :depends (dash f)
       :type github
       :pkgname "Fuco1/dired-hacks"
       :features ("dired-subtree")
       :post-init (progn
                    (add-hook 'dired-mode-hook '(lambda()
                                                  (message "inside dired-hacks hook")
                                                  (define-key dired-mode-map (kbd "i") 'dired-subtree-insert)
                                                  (define-key dired-mode-map (kbd ";") 'dired-subtree-remove)
                                                  ))
                    )
       )
