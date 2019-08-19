(:name vimrc-mode
       :description "Enables syntax highlighting for .vimrc/_vimrc files"
       :type github
       :pkgname "mcandre/vimrc-mode"
       :prepare (progn
                  (add-to-list 'auto-mode-alist
                               '(".vim\\(rc\\|peratorrc\\)?$" . vimrc-mode))
                  (add-to-list 'auto-mode-alist
                               '(".vifm\\(rc\\|peratorrc\\)?$" . vimrc-mode))
))
