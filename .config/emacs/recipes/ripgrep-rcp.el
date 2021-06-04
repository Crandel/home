;;; ripgrep-rcp.el --- Packages for ripgrep

;;; Code:
;; (use-package helm-ag
;;   :ensure t
;;   :defer t
;;   :custom
;;   (helm-ag-insert-at-point 'word)
;;   (helm-ag-base-command    "rg --color=never -i --vimgrep")
;;   (helm-ag-use-temp-buffer t)
;;   (helm-ag-fuzzy-match     t)
;;   :bind
;;   ("C-x g" . helm-do-ag-project-root)
;; )

(use-package ripgrep
  :ensure t
  :defer t)

(provide 'ripgrep-rcp)

;;; Commentary:
;;
;;; ripgrep-rcp.el ends here
