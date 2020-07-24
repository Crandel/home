(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode)
  (git-gutter:linum-setup)
  :bind
  ("C-c [" . git-gutter:next-hunk)
  ("C-c ]" . git-gutter:previous-hunk)
)

(provide 'git-gutter-rcp)
