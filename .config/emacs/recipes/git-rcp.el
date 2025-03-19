;;; git-rcp.el --- Packages, related to the version control system Git

;;; Code:
(eval-when-compile (require 'use-package))
(use-package magit
  :ensure t
  :defer t
  :commands magit-status
  :custom-face
  (magit-diff-added ((t (:background "dark slate gray" :foreground "chocolate"))))
  (magit-diff-added-highlight ((t (:background "dark olive green" :foreground "gold"))))
  (magit-diff-removed ((t (:background "red" :foreground "#ffdddd"))))
  (magit-diff-removed-highlight ((t (:background "dark red" :foreground "navajo white"))))
  :custom
  (magit-diff-refine-hunk                'all)
  (magit-display-buffer-function         'magit-display-buffer-fullframe-status-v1)
  (magit-ediff-dwim-show-on-hunks        t)
  (magit-format-file-function            'magit-format-file-nerd-icons)
  (magit-log-arguments                   '("--graph" "--color" "--decorate" "--show-signature" "--follow" "-n256"))
  (magit-log-margin-show-committer-date  t)
  (magit-log-remove-graph-args           '("--follow" "--grep" "-G" "-S" "-L"))
  (magit-todos-insert-after              '(bottom))
  :bind(
  ("C-c t g" . magit-status)
  (:map magit-diff-mode-map
        ("k" . nil)
        ("x" . magit-delete-thing))
  (:map magit-status-mode-map
        ("TAB" . magit-section-toggle)
        ("x"   . magit-discard)
        ("p"   . magit-push))
  )
)

(use-package magit-todos
  :ensure t
  :after magit
)

(use-package git-modes
  :mode "/.dockerignore\\'"
  :ensure t
)

(use-package magit-delta
  :ensure t
  :custom
  (magit-delta-default-dark-theme "gruvbox-dark")
  :hook (magit-mode . magit-delta-mode))

(use-package git-gutter
  :ensure t
  :defer t
  :config
  (global-git-gutter-mode)
  :bind
  ("C-c [" . git-gutter:next-hunk)
  ("C-c ]" . git-gutter:previous-hunk)
)

(use-package diff-hl
  :ensure t
  :defer t
  :hook
  (prog-mode . diff-hl-mode)
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
)

(provide 'git-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; git-rcp.el ends here
