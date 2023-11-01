;;; sql-rcp.el --- SQL mode settings

;;; Code:
(use-package sql-mode
  :mode "\\.pgsql\\'"
  :custom
  (sql-product 'postgres)
)

(use-package sqlformat
  :ensure t)

(provide 'sql-rcp)
;;; Commentary:
;;
;;; sql-rcp.el ends here
