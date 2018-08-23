(:name flycheck-rust
       :description "Flycheck Rust additions and Cargo support."
       :type github
       :pkgname "flycheck/flycheck-rust"
       :depends (dash flycheck seq)
       :post-init (eval-after-load 'rust-mode
                    '(add-hook 'flycheck-mode-hook 'flycheck-rust-setup)))
