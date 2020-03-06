(:name restclient
      :description "HTTP REST client tool for emacs"
      :type github
      :pkgname "pashky/restclient.el"
      :post-init (progn
                   (add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))
                   (add-hook 'restclient-mode-hook '(lambda()
                                                      (eval-after-load "company"
                                                        '(progn
                                                           (my-change-company-backends 'company-restclient)
                                                           ))
                                                      ))
                   ))
