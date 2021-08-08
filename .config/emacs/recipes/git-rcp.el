;;; git-gutter-rcp.el --- Show git changes in buffer

;;; Code:
(use-package gitconfig-mode
  :ensure t
)

(use-package gitignore-mode
  :ensure t
  :mode "/.dockerignore\\'"
)

(use-package gitattributes-mode
  :ensure t
)

(use-package git-gutter
  :ensure t
  :defer t
  :config
  (global-git-gutter-mode)
  (git-gutter:linum-setup)
  :bind
  ("C-c [" . git-gutter:next-hunk)
  ("C-c ]" . git-gutter:previous-hunk)
)

(provide 'git-rcp)
;;; Commentary:
;;
;;; git-gutter-rcp.el ends here
