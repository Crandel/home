;;; orderless-rcp.el --- This package provides an orderless completion style that divides the pattern into space-separated components,
;;; and matches candidates that match all of the components in any order.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package orderless
  :ensure t
  :defer 1
  :preface
  (defun vd/orderless-dispatch (pattern _index _total)
    "Recognizes the following patterns:
 * =literal literal=
 * `initialism initialism`
 * !without-literal without-literal!
 * .ext (file extension)
 * regexp$ (regexp matching at end)
 * ~flex flex~"
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern) `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
     ;; File extensions
     ;; ((string-match-p "\\`\\.." pattern) `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x100000-\x10FFFD]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ((string-suffix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 0 -1)))
     ;; Initialism matching
     ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))
  (defun vd/just-one-face (fn &rest args)
    (let ((orderless-match-faces [completions-common-part]))
      (apply fn args)))
  (defun vd/match-components-literally ()
    "Components match literally for the rest of the session."
    (interactive)
    (setq-local orderless-matching-styles '(orderless-literal)
                orderless-style-dispatchers nil))
  :config
  (setq completion-styles             '(orderless partial-completion)
        completion-category-defaults  nil
        completion-category-overrides nil
        )
  (advice-add 'company-capf--candidates :around #'vd/just-one-face)
  :custom
  (orderless-component-separator "[ &]")
  (orderless-matching-styles     'orderless-regexp)
  (orderless-style-dispatchers   '(vd/orderless-dispatch))
  :bind
  (:map minibuffer-local-completion-map
        ("C-l" . vd/match-components-literally)
        )
)

(provide 'orderless-rcp)
;;; Commentary:
;;
;;; orderless-rcp.el ends here
