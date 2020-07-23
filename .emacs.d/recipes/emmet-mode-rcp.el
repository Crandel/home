(:name emmet-mode
       :website "https://github.com/smihica/emmet-mode"
       :description "Produce HTML from CSS-like selectors."
       :type "github"
       :pkgname "smihica/emmet-mode"
       :post-init (progn
                    (add-hook 'sgml-mode-hook 'emmet-mode)
                    (add-hook 'html-mode-hook 'emmet-mode)
                    (add-hook 'web-mode-hook  'emmet-mode)
                    (add-hook 'css-mode-hook  'emmet-mode)))
