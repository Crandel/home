;;; beacon-rcp.el --- A light that follows your cursor around so you don't lose it!

;;; Code:
(eval-when-compile (require 'use-package))
(use-package beacon
  :ensure t
  :defer 1
  :init
  (beacon-mode 1)
)

(provide 'beacon-rcp)


;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; beacon-rcp.el ends here
