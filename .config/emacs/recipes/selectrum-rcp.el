;;; selectrum-rcp.el --- Selectrum aims to provide a better completion UI using standard Emacs APIs.
;;;In essence it is an interface for selecting items from a list.

;;; Code:
(use-package selectrum
  :ensure t
  :demand t
  :custom
  (selectrum-display-action '(display-buffer-show-in-posframe))
  :init
  (defun display-buffer-show-in-posframe (buffer _alist)
    (frame-root-window
     (posframe-show buffer
                    :min-height 10
                    :min-width (frame-width)
                    :internal-border-width 1
                    :left-fringe 8
                    :right-fringe 8
                    :poshandler 'posframe-poshandler-frame-bottom-left-corner)))
  :config
  (selectrum-mode +1)
  :hook
  (minibuffer-exit . posframe-delete-all)
  :bind
  (:map selectrum-minibuffer-map
        ([right] . selectrum-insert-current-candidate)
        ([left] . selectrum-backward-kill-sexp)
   )
)

(use-package posframe
  :ensure t
  :demand t
)

(provide 'selectrum-rcp)
;;; Commentary:
;;
;;; selectrum-rcp.el ends here
