;;; mini-frame-rcp.el --- Place minibuffer at the top of the current frame on read-from-minibuffer.

;;; Code:
(use-package mini-frame
  :ensure t
  :custom
  (mini-frame-show-parameters '((top . 10)
                                (width . 0.7)
                                (left . 0.5)))
  (x-gtk-resize-child-frames  'resize-mode)
  (mini-frame-detach-on-hide  nil)
  :config
  (mini-frame-mode +1)
)

(provide 'mini-frame-rcp)
;;; Commentary:
;;
;;; mini-frame-rcp.el ends here
