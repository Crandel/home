;;; projectile-rcp.el --- Project manager for emacs

;;; Code:
(eval-when-compile (require 'use-package))
(use-package projectile
  :ensure t
  :functions (projectile-project-root projectile-command-map)
  :defer 0.5
  :defines projectile-globally-ignored-directories
  :config
  (projectile-mode)
  :bind
  ("C-c p" . projectile-command-map)
)

(provide 'projectile-rcp)

;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; projectile-rcp.el ends here
