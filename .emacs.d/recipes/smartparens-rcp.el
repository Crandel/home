(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode)
  (sp-with-modes '(markdown-mode gfm-mode rst-mode)
                 (sp-local-pair "*" "*")
                 (sp-local-pair "**" "**")
                 (sp-local-pair "_" "_" ))
  (sp-with-modes '(web-mode)
                 (sp-local-pair "%" "%")
                 (sp-local-pair "<" ">"))
  :bind (
  ("C-c w" . nil)
  ("C-c w" . sp-rewrap-sexp)
  ("C-c r" . sp-unwrap-sexp)
  ("C-c f" . sp-forward-sexp)
  ("C-c d" . sp-backward-sexp))
  :chords (
  ("''" . sp-rewrap-sexp))
)

(provide 'smartparens-rcp)
