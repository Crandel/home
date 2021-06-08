;;; corfu-rcp.el --- Corfu enhances the default completion in region function with a completion overlay.

;;; Code:
(use-package corfu
  :ensure t
  :init
  (corfu-global-mode)
  :custom
  (corfu-cycle t)
)

(provide 'corfu-rcp)
;;; Commentary:
;;
;;; corfu-rcp.el ends here
