;;; expand-region-rcp.el --- Increase selected region by semantic units.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package expand-region
  :ensure t
  :defer t
  :bind
  ("C-c ." . er/expand-region)
  :chords
  ("//" . er/expand-region)
)

(provide 'expand-region-rcp)
;;; Commentary:
;;
;;; expand-region-rcp.el ends here
