;;; chezmoi-rcp.el --- An emacs package for interacting with chezmoi.

;;; Code:

(eval-when-compile (require 'use-package))
(use-package chezmoi
  :ensure t
  :defer 2
  :functions (chezmoi-write chezmoi-find)
  :custom
  (chezmoi-template-display-p t)
  :hook
  (evil-mode . (lambda ()
    (evil-global-set-key 'normal "grw" 'chezmoi-write)
    (evil-global-set-key 'normal "grf" 'chezmoi-find)))
)

(provide 'chezmoi-rcp)

;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; chezmoi-rcp.el ends here
