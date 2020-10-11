;;; terraform-rcp.el --- Terraform mode

;;; Code:
(use-package terraform-mode
  :mode "\\.tf\\'"
  :ensure t
  :defer t
)

(use-package terraform-doc
  :ensure t
  :defer t
)

(use-package company-terraform
  :ensure t
  :defer t
  :hook
  (terraform-mode . company-terraform-init)
)

(provide 'terraform-rcp)
;;; Commentary:
;;
;;; terraform-rcp.el ends here
