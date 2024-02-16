;;; hydra-rcp.el ---     Make bindings that stick around.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package hydra
  :ensure t
  :commands defhydra
)

(provide 'hydra-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; hydra-rcp.el ends here
