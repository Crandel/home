;;; highlight-indentation-rcp.el --- Minor modes for highlighting indentation

;;; Code:
(use-package highlight-indentation
  :ensure t
  :defer t
  :custom-face
  (highlight-indentation-face ((t (:background "#1d2021"
                                   :foreground "IndianRed"))))
  :hook
  ((c++-mode
    c-mode
    emacs-lisp-mode
    fish-mode
    java-mode
    js-mode
    lisp-interaction-mode
    markdown-mode
    python-mode
    rust-mode
    scala-mode
    sh-mode
    web-mode
    yaml-mode) . highlight-indentation-mode)
)

(provide 'highlight-indentation-rcp)
;;; Commentary:
;;
;;; highlight-indentation-rcp.el ends here
