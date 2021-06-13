;;; selectrum-rcp.el --- Selectrum aims to provide a better completion UI using standard Emacs APIs.
;;;In essence it is an interface for selecting items from a list.

;;; Code:
(use-package selectrum
  :ensure t
  :demand t
  :config
  (selectrum-mode +1)
  :bind
  (:map selectrum-minibuffer-map
        ([right] . selectrum-insert-current-candidate)
        ([left] . selectrum-backward-kill-sexp)
   )
)

(provide 'selectrum-rcp)
;;; Commentary:
;;
;;; selectrum-rcp.el ends here
