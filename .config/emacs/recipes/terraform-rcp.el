;;; terraform-rcp.el --- Terraform mode

;;; Code:
(use-package terraform-mode
  :mode "\\.tf\\'"
  :ensure t)
(use-package terraform-doc
  :ensure t)
(use-package company-terraform
  :ensure t
  :hook
  (terraform-mode . company-terraform-init))

(provide 'terraform-rcp)

;;; Commentary:
;;
;;; terraform-rcp.el ends here
