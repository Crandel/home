;;; projectile-rcp.el --- Project manager for emacs

;;; Code:
(eval-when-compile (require 'use-package))
(use-package projectile
  :ensure t
  :functions (projectile-project-root projectile-command-map)
  :defer 0.5
  :defines projectile-globally-ignored-directories
  :custom
  (projectile-run-use-comint-mode t)
  :config
  (projectile-mode)
  :bind(
  ("C-c p" . projectile-command-map)
  :map evil-normal-state-map
  ("gp"  . projectile-command-map)
  :map projectile-command-map
  ("xc" . projectile-run-command-in-root)
  )
)

(provide 'projectile-rcp)

;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; projectile-rcp.el ends here
