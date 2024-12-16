;;; sql-rcp.el --- SQL mode settings

;;; Code:
(eval-when-compile (require 'use-package))
(use-package sql-mode
  :mode "\\.pgsql\\'"
  :custom
  (sql-product 'postgres)
)

(use-package sql-indent
  :ensure t
)

(use-package sqlformat
  :ensure t)

(use-package sqlite-mode-extras
  :ensure t
  :bind (:map
         sqlite-mode-map
         ("n" . next-line)
         ("p" . previous-line)
         ("b" . sqlite-mode-extras-backtab-dwim)
         ("f" . sqlite-mode-extras-tab-dwim)
         ("+" . sqlite-mode-extras-add-row)
         ("D" . sqlite-mode-extras-delete-row-dwim)
         ("C" . sqlite-mode-extras-compose-and-execute)
         ("E" . sqlite-mode-extras-execute)
         ("S" . sqlite-mode-extras-execute-and-display-select-query)
         ("R" . sqlite-mode-extras-delete-row-dwim)
         ("g" . sqlite-mode-extras-refresh)
         ("DEL" . sqlite-mode-extras-backtab-dwim)
         ("TAB" . sqlite-mode-extras-tab-dwim)
         ("RET" . sqlite-mode-extras-ret-dwim)))

(provide 'sql-rcp)
;;; Commentary:
;;
;;; sql-rcp.el ends here
