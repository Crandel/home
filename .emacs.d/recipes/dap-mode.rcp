(:name dap-mode
       :description "Debug Adapter Protocol mode"
       :type github
       :pkgname "emacs-lsp/dap-mode"
       :load-path "."
       :depends (bui dash f lsp-mode s tree-mode treemacs)
       :post-init (progn
                    (add-hook 'java-mode-hook '(lambda()
                         (dap-mode 1)
                         (dap-ui-mode 1)
                         (require 'dap-java)
                         (dap-register-debug-template "Java testApp"
                                                      (list :type "java"
                                                            :request "launch"
                                                            :args ""
                                                            :vmArgs "-ea -DtestApp.instance.name=testapp_1"
                                                            :projectName "testApp"
                                                            :mainClass "testApp.Main"
                                                            :env '(("DEV" . "1"))))))
                    (add-hook 'python-mode-hook '(lambda()
                         (dap-mode 1)
                         (dap-ui-mode 1)
                         (require 'dap-python)
                         (dap-register-debug-template "Python AddressBook"
                                                      (list :type "python"
                                                            :args "-i"
                                                            :cwd nil
                                                            :env '(("DEBUG" . "1"))
                                                            :target-module (expand-file-name "~/projects/python/address_book")
                                                            :request "launch"
                                                            :name "AddressBook"))))
                    ))
