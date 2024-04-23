;;; scala-rcp.el --- Scala support

;;; Code:
(use-package sbt-mode
  :ensure t
  :defer t
  :config
  (substitute-key-definition 'minibuffer-complete-word
                             'self-insert-command
                             minibuffer-local-completion-map)
)

(use-package scala-mode
  :ensure t
  :defer t
  :mode ("\\.sc\\'" "\\.scala\\'")
  :custom
  (scala-basic-offset               2)
  (scala-indent:use-javadoc-style   t)
  (scala-indent:align-parameters    t)
  (max-lisp-eval-depth              50000)
  (max-specpdl-size                 5000)
  :bind (
    :map scala-mode-map
         ("RET" . reindent-then-newline-and-indent)
         ("M-RET" . newline))
)

(use-package lsp-metals
  :ensure t
  :defer t
)

(provide 'scala-rcp)

;;; Commentary:
;;; scala-rcp.el ends here
