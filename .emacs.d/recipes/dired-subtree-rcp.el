(use-package dired-subtree
  :ensure t
  :after (dired)
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove))
)

(provide 'dired-subtree-rcp)
