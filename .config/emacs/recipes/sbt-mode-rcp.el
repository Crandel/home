(:name sbt-mode
       :description "An emacs mode for interacting with scala sbt and projects"
       :type github
       :pkgname "ensime/emacs-sbt-mode"
       :post-init (substitute-key-definition
                   'minibuffer-complete-word
                   'self-insert-command
                   minibuffer-local-completion-map))
