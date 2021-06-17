;;; marginalia-rcp.el --- This package provides marginalia-mode which adds marginalia to the minibuffer completions.

;;; Code:
(use-package marginalia
  :ensure t
  :after (:any consult vertico)
  :config
  (marginalia-mode)
)

(provide 'marginalia-rcp)
;;; Commentary:
;;
;;; marginalia-rcp.el ends here
