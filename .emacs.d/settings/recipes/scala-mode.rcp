(:name scala-mode
       :description "The definitive scala-mode for emacs"
       :type github
       :website "http://ensime.org"
       :pkgname "ensime/emacs-scala-mode"
       :post-init (progn
                    (add-to-list 'auto-mode-alist '("\\.sc\\'" . scala-mode))
                    (setq scala-indent:use-javadoc-style   t
                          scala-indent:align-parameters    t
                          max-lisp-eval-depth              50000
                          max-specpdl-size                 5000)
                    (add-hook 'scala-mode-hook '(lambda()
                                                  (define-key scala-mode-map (kbd "RET") 'newline-and-indent)
                                                  (define-key scala-mode-map (kbd "M-RET") 'newline)
                                                  )
                              )
                    ))
