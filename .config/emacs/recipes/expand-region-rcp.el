;;; expand-region-rcp.el --- Increase selected region by semantic units.

;;; Commentary:
;; 

;;; Code:

(use-package expand-region
  :ensure t
  :bind
  ("C-c ." . er/expand-region)
  :chords
  ("//" . er/expand-region)
)

(provide 'expand-region-rcp)

;;; expand-region-rcp.el ends here
