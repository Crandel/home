;;; mini-frame-rcp.el --- Place minibuffer at the top of the current frame on read-from-minibuffer.

;;; Code:
(eval-when-compile (require 'use-package))
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
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; mini-frame-rcp.el ends here
