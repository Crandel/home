;;; k8s-mode-rcp.el --- Kubernetes configuration mode

;;; Code:
(use-package k8s-mode
  :ensure t
  :mode (
         "**/helm/**.*\\.yaml$"
         "\\*kubernetes pod\\*"
         )
  :hook (k8s-mode . yas-minor-mode))


(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

;; (use-package kubernetes-helm
;;   :ensure t
;; )

(provide 'k8s-mode-rcp)
;;; Commentary:
;;
;;; k8s-mode-rcp.el ends here
