;;; dired-rcp.el --- Dired mode.
;;; Code:
(eval-when-compile (require 'use-package))

(use-package dired
  :defer t
  :custom
  (dired-dwim-target                        t "guess a target directory")
  (dired-auto-revert-buffer                 t)
  (dired-kill-when-opening-new-dired-buffer t "kill the current buffer when selecting a new directory")
  (dired-listing-switches                   "-AhlF --time-style=long-iso --group-directories-first")
  :bind (:map dired-mode-map
              ("H" . dired-up-directory)
              ("-" . dired-do-hardlink)
              )
  )

(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :custom
  (dired-sidebar-theme                'icons)
  (dired-sidebar-display-alist        '((side . right) (slot . -1)))
  :bind
  ([f7] . dired-sidebar-toggle-sidebar)
  (:map dired-sidebar-mode-map
        ("H" . dired-sidebar-up-directory)
        ("-" . dired-do-hardlink)
        )
)

(use-package dired-subtree
  :ensure t
  :defer t
  :after (dired)
  :bind (:map dired-mode-map
              ("<backtab>" . dired-subtree-toggle)
              ("i"         . dired-subtree-insert)
              (";"         . dired-subtree-remove))
)

(use-package all-the-icons-dired
  :ensure t
  :hook
  (dired-mode . all-the-icons-dired-mode)
)

(provide 'dired-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; dired-rcp.el ends here
