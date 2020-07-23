(use-package magit
  :ensure t
  :bind
  ("C-x C-z" . 'magit-status)
  (:map magit-file-mode-map
        ("C-x g" . nil))
  :custom
    (magit-log-arguments '("--graph" "--color" "--decorate" "--show-signature" "--follow" "-n256"))
    (magit-log-margin-show-committer-date  t)
    (magit-ediff-dwim-show-on-hunks        t)
    (magit-log-remove-graph-args           '("--follow" "--grep" "-G" "-S" "-L"))
)

(provide 'magit-rcp)
