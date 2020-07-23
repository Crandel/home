(:name markdown-mode
       :description "Major mode to edit Markdown files in Emacs"
       :website "http://jblevins.org/projects/markdown-mode/"
       :type github
       :pkgname "jrblevin/markdown-mode"
       :prepare (add-to-list 'auto-mode-alist
                             '("\\.\\(md\\|mdown\\|markdown\\)\\'" . markdown-mode))
       :post-init (eval-after-load "markdown-mode"
                    '(progn
                      (define-key markdown-mode-map (kbd "M-n") 'mc/mark-next-like-this)
                      (define-key markdown-mode-map (kbd "M-m") 'mc/mark-previous-like-this)
                        )))

