(:name rust-mode
       :type github
       :pkgname "rust-lang/rust-mode"
       :description "Emacs mode for Rust"
       :post-init (progn
                     (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
                     (setq rust-format-on-save t)
))
