;;; vertico-rcp.el --- Vertico provides a minimalistic vertical completion UI, which is based on the default completion system.

;;; Code:
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :bind
  (:map vertico-map
        ([right] . vertico-insert)
        ([left] . backward-kill-word)
        )
  :init
  (vertico-mode)
)

(provide 'vertico-rcp)

;;; Commentary:
;;
;;; vertico-rcp.el ends here
