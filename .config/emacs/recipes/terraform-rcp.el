;;; terraform-rcp.el --- Terraform mode

;;; Code:
(eval-when-compile (require 'use-package))
(use-package terraform-mode
  :mode "\\.tf\\'"
  :ensure t
  :defer t
)

(use-package terraform-doc
  :ensure t
  :defer t
)

(provide 'terraform-rcp)
;;; Commentary:
;;
;;; terraform-rcp.el ends here
