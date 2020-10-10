;;; magit-rcp.el --- Magit is an interface to the version control system Git

;;; Code:
(use-package magit
  :ensure t
  :defer t
  :custom-face
  (magit-diff-added ((t (:background "dark slate gray" :foreground "chocolate"))))
  (magit-diff-added-highlight ((t (:background "dark olive green" :foreground "gold"))))
  (magit-diff-removed ((t (:background "red" :foreground "#ffdddd"))))
  (magit-diff-removed-highlight ((t (:background "dark red" :foreground "navajo white"))))
  :custom
  (magit-log-arguments '("--graph" "--color" "--decorate" "--show-signature" "--follow" "-n256"))
  (magit-log-margin-show-committer-date  t)
  (magit-ediff-dwim-show-on-hunks        t)
  (magit-log-remove-graph-args           '("--follow" "--grep" "-G" "-S" "-L"))
  :bind
  ("C-x C-z" . 'magit-status)
  (:map magit-file-mode-map
        ("C-x g" . nil))
)

(provide 'magit-rcp)
;;; Commentary:
;;
;;; magit-rcp.el ends here
