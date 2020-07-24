(use-package rainbow-delimiters
  :ensure t
  :hook
  (emacs-lisp-mode       . rainbow-delimiters-mode)
  (js2-mode              . rainbow-delimiters-mode)
  (json-mode             . rainbow-delimiters-mode)
  (lisp-interaction-mode . rainbow-delimiters-mode)
  (rust-mode             . rainbow-delimiters-mode)
  (scala-mode            . rainbow-delimiters-mode)
  (sh-mode               . rainbow-delimiters-mode)
  (sql-mode              . rainbow-delimiters-mode)
  (web-mode              . rainbow-delimiters-mode)
)

(use-package rainbow-mode
  :ensure t
  :hook
  (conf-mode             . rainbow-mode)
  (css-mode              . rainbow-mode)
  (emacs-lisp-mode       . rainbow-mode)
  (javascript2-mode      . rainbow-mode)
  (lisp-interaction-mode . rainbow-mode)
  (sh-mode               . rainbow-mode)
  (web-mode              . rainbow-mode)
)

(provide 'rainbow-rcp)
