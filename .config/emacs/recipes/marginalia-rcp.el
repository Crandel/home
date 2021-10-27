;;; marginalia-rcp.el --- This package provides marginalia-mode which adds marginalia to the minibuffer completions.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package marginalia
  :ensure t
  :after (:any consult vertico)
  :config
  (marginalia-mode)
)

(provide 'marginalia-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; marginalia-rcp.el ends here
