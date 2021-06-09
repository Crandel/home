;;; helpful-rcp.el --- Helpful is an alternative to the built-in Emacs help that provides much more contextual information.

;;; Code:
(use-package helpful
  :ensure t
  :defer t
  :bind
  ([remap describe-callable]    . helpful-callable)
  ([remap describe-function]    . helpful-function)
  ([remap describe-variable]    . helpful-variable)
  ([remap describe-key]         . helpful-key)
  ([remap view-emacs-debugging] . helpful-at-point)
)

(provide 'helpful-rcp)
;;; Commentary:
;;
;;; helpful-rcp.el ends here
