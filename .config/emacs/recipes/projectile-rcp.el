;;; projectile-rcp.el --- Project manager for emacs

;;; Code:
(use-package projectile
  :ensure t
  :defer t
  :defines projectile-globally-ignored-directories
  :config
  (projectile-mode)
  (add-to-list 'projectile-globally-ignored-directories "*bloop")
  (add-to-list 'projectile-globally-ignored-directories "*metals")
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
)

(provide 'projectile-rcp)

;;; Commentary:
;;
;;; projectile-rcp.el ends here
