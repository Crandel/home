;;; orderless-rcp.el --- This package provides an orderless completion style that divides the pattern into space-separated components,
;;; and matches candidates that match all of the components in any order.

;;; Code:
(use-package orderless
  :ensure t
  :defer t
  :init
  (setq completion-styles             '(orderless)
        completion-category-defaults  nil
        completion-category-overrides '((file (styles . (partial-completion)))))
  :custom
  (orderless-matching-styles     '(orderless-literal orderless-regexp))
)

(provide 'orderless-rcp)
;;; Commentary:
;;
;;; orderless-rcp.el ends here
