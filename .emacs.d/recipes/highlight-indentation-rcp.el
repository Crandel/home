(use-package highlight-indentation
  :ensure t
  :custom-face
  (highlight-indentation-face ((t (:foreground "IndianRed")
                                  (:background "#1d2021"))))
  :hook
  (c++-mode              . highlight-indentation-mode)
  (c-mode                . highlight-indentation-mode)
  (emacs-lisp-mode       . highlight-indentation-mode)
  (fish-mode             . highlight-indentation-mode)
  (java-mode             . highlight-indentation-mode)
  (js-mode               . highlight-indentation-mode)
  (lisp-interaction-mode . highlight-indentation-mode)
  (markdown-mode         . highlight-indentation-mode)
  (python-mode           . highlight-indentation-mode)
  (rust-mode             . highlight-indentation-mode)
  (scala-mode            . highlight-indentation-mode)
  (sh-mode               . highlight-indentation-mode)
  (web-mode              . highlight-indentation-mode)
  (yaml-mode             . highlight-indentation-mode)
)

(provide 'highlight-indentation-rcp)
