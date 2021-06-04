;;; k8s-mode-rcp.el --- Kubernetes configuration mode

;;; Code:
(use-package k8s-mode
  :ensure t
  :mode (
         ".config.yaml"
         "Chart.yaml"
         "_helpers.tpl"
         "configmap.yaml"
         "deployment.yaml"
         "\\*kubernetes pod\\*"
         "ingress.yaml"
         "pvc.yaml"
         "service.yaml"
         "statefulset.yaml"
         "values.yaml"
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
