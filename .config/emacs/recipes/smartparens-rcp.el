;;; smartparens-rcp.el --- Smartparens is a minor mode for dealing with pairs in Emacs.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package smartparens
  :ensure t  ;; install the package
  :hook ((prog-mode text-mode markdown-mode) . smartparens-mode) ;; add `smartparens-mode` to these hooks
  :preface
  (defun vd/wrap-parens ()
    (interactive)
    (sp-wrap-with-pair "("))
  (defun vd/wrap-brackets ()
    (interactive)
    (sp-wrap-with-pair "["))
  (defun vd/wrap-braces ()
    (interactive)
    (sp-wrap-with-pair "{"))
  (defun vd/wrap-quotes ()
    (interactive)
    (sp-wrap-with-pair "'"))
  (defun vd/wrap-dquotes ()
    (interactive)
    (sp-wrap-with-pair "\""))
  (defun vd/wrap-bquotes ()
    (interactive)
    (sp-wrap-with-pair "`"))
  (defun vd/wrap-underscores ()
    (interactive)
    (sp-wrap-with-pair "_"))
  (defun vd/wrap-minuses ()
    (interactive)
    (sp-wrap-with-pair "-"))
  (defun vd/wrap-equals ()
    (interactive)
    (sp-wrap-with-pair "="))
  :config
  ;; load default config
  (require 'smartparens-config)
  :bind(
  ("C-c a f"  . sp-forward-sexp)
  ("C-c a b"  . sp-backward-sexp)
  ("C-c a a"  . vd/wrap-bquotes)
  ("C-c a c"  . vd/wrap-braces)
  ("C-c a d"  . vd/wrap-dquotes)
  ("C-c a e"  . vd/wrap-equals)
  ("C-c a m"  . vd/wrap-minuses)
  ("C-c a q"  . vd/wrap-quotes)
  ("C-c a r"  . vd/wrap-parens)
  ("C-c a s"  . vd/wrap-brackets)
  ("C-c a u"  . vd/wrap-underscores)
  ("C-c a w"  . sp-unwrap-sexp)
  )
)


(provide 'smartparens-rcp)
;;; Commentary:
;;
;;; smartparens-rcp.el ends here
