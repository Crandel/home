(:name helm-ag
       :description "The silver search with helm interface."
       :type github
       :pkgname "syohex/emacs-helm-ag"
       :depends (helm)
       :post-init (progn
                    (global-set-key (kbd "C-x g") 'helm-do-ag-project-root)

                    (setq helm-ag-insert-at-point 'word
                          helm-ag-base-command "rg --color=never -i --vimgrep"
                          helm-ag-use-temp-buffer t
                          helm-ag-fuzzy-match     t)))
