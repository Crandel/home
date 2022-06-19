;;; dired-rcp.el --- Dired mode.
;;; Code:
(eval-when-compile (require 'use-package))

(use-package dired
  :defer t
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal dired-mode-map "H" 'dired-up-directory))
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
  (dired-sidebar-use-term-integration t)
  (dired-sidebar-use-custom-font      t)
  (dired-sidebar-display-alist        '((side . right) (slot . -1)))
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal dired-sidebar-mode-map "H" 'dired-sidebar-up-directory))
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
)

(provide 'dired-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; dired-rcp.el ends here
