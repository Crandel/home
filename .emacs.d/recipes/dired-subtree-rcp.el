;;; dired-subtree-rcp.el --- Insert subdirectories in a tree-like fashion

;;; Commentary:
;; 

;;; Code:

(use-package dired-subtree
  :ensure t
  :after (dired)
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove))
)

(provide 'dired-subtree-rcp)

;;; dired-subtree-rcp.el ends here
