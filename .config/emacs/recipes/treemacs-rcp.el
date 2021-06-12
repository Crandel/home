;;; treemacs-rcp.el --- Treemacs support

;;; Code:
(use-package treemacs
  :ensure t
  :defer t
  :bind
  ([f7] . treemacs)
  :custom
  (treemacs-deferred-git-apply-delay      0.5)
  (treemacs-directory-name-transformer    #'identity)
  (treemacs-display-in-side-window        t)
  (treemacs-eldoc-display                 t)
  (treemacs-file-event-delay              5000)
  ;(treemacs-file-extension-regex          treemacs-last-period-regex-value)
  (treemacs-file-follow-delay             0.2)
  (treemacs-file-name-transformer         #'identity)
  (treemacs-follow-after-init             t)
  (treemacs-git-command-pipe              "")
  (treemacs-goto-tag-strategy             'refetch-index)
  (treemacs-indentation                   2)
  (treemacs-indentation-string            " ")
  (treemacs-is-never-other-window         nil)
  (treemacs-max-git-entries               5000)
  (treemacs-missing-project-action        'ask)
  (treemacs-no-png-images                 nil)
  (treemacs-no-delete-other-windows       t)
  (treemacs-project-follow-cleanup        nil)
  (treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory))
  (treemacs-position                      'left)
  (treemacs-read-string-input             'from-minibuffer)
  (treemacs-recenter-distance             0.1)
  (treemacs-recenter-after-file-follow    nil)
  (treemacs-recenter-after-tag-follow     nil)
  (treemacs-recenter-after-project-jump   'always)
  (treemacs-recenter-after-project-expand 'on-distance)
  (treemacs-show-cursor                   t)
  (treemacs-show-hidden-files             t)
  (treemacs-silent-filewatch              nil)
  (treemacs-silent-refresh                nil)
  (treemacs-sorting                       'alphabetic-asc)
  (treemacs-space-between-root-nodes      t)
  (treemacs-tag-follow-cleanup            t)
  (treemacs-tag-follow-delay              1.5)
  (treemacs-user-mode-line-format         nil)
  (treemacs-width                         35)
)

(use-package treemacs-icons-dired
  :ensure t
  :hook (dired-mode . treemacs-icons-dired-mode)
)

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile)
)

(use-package treemacs-magit
  :ensure t
  :after (treemacs magit)
)

(use-package treemacs-all-the-icons
  :ensure t
  :after treemacs
)
(provide 'treemacs-rcp)
;;; Commentary:
;;

;;; treemacs-rcp.el ends here
