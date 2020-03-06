(:name git-gutter
       :description "Emacs port of GitGutter Sublime Text 2 Plugin"
       :website "https://github.com/syohex/emacs-git-gutter"
       :type github
       :pkgname "syohex/emacs-git-gutter"
       :post-init (progn
                    (global-set-key (kbd "C-c [") 'git-gutter:next-hunk)
                    (global-set-key (kbd "C-c ]") 'git-gutter:previous-hunk)

                    (global-git-gutter-mode t)
                    (git-gutter:linum-setup)))
