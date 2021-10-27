;;; emacs-dashboard-rcp.el --- An extensible emacs startup screen showing you what’s most important.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-projects-backend  'projectile)
  (dashboard-items             '(
                                 (bookmarks . 1)
                                 (recents   . 5)
                                 (projects  . 5)
                                 ))
  (dashboard-set-footer        nil)
  :bind (
  (:map dashboard-mode-map
        ("C-p" . nil))
  )
)

(provide 'emacs-dashboard-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; emacs-dashboard-rcp.el ends here
