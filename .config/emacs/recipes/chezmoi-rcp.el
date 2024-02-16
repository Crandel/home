;;; chezmoi-rcp.el --- An emacs package for interacting with chezmoi.

;;; Code:

(eval-when-compile (require 'use-package))
(use-package chezmoi
  :ensure t
  :defer 2
  :commands (chezmoi-write chezmoi-find)
  :custom
  (chezmoi-template-display-p t)
  :bind
  ("C-c t w" . chezmoi-write)
  ("C-c t f" . chezmoi-find)
)

(provide 'chezmoi-rcp)

;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; chezmoi-rcp.el ends here
