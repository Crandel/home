;;; mo-git-blame-rcp.el --- Blame git changes in buffer

;;; Commentary:
;; 

;;; Code:

(use-package mo-git-blame
  :ensure t
  :bind
  ("C-c g" . mo-git-blame-current)
)

(provide 'mo-git-blame-rcp)

;;; mo-git-blame-rcp.el ends here
