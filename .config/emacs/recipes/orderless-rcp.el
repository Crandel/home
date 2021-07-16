;;; orderless-rcp.el --- This package provides an orderless completion style that divides the pattern into space-separated components,
;;; and matches candidates that match all of the components in any order.

;;; Code:
(use-package orderless
  :ensure t
  :defer 1
  :config
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
     ((string-match-p "\\`\\.." pattern) `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x100000-\x10FFFD]*$")))
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
  (setq completion-styles             '(orderless)
        completion-category-defaults  nil
        completion-category-overrides '((file (styles . (partial-completion))))
        orderless-style-dispatchers '(vd/orderless-dispatch))
)

(provide 'orderless-rcp)
;;; Commentary:
;;
;;; orderless-rcp.el ends here
