;;; scala-rcp.el --- Scala support

;;; Commentary:
;; 

;;; Code:

(use-package sbt-mode
  :ensure t
  :config
  (substitute-key-definition 'minibuffer-complete-word
                             'self-insert-command
                             minibuffer-local-completion-map)
)

(use-package scala-mode
  :ensure t
  :mode ("\\.sc\\'" "\\.scala\\'")
  :custom
  (scala-indent:use-javadoc-style   t)
  (scala-indent:align-parameters    t)
  (max-lisp-eval-depth              50000)
  (max-specpdl-size                 5000)
  :bind (
    :map scala-mode-map
         ("RET" . newline-and-indent)
         ("M-RET" . newline))
)

(use-package lsp-metals :ensure t)

(provide 'scala-rcp)

;;; scala-rcp.el ends here
