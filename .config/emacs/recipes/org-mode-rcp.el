;;; org-mode-rcp.el --- A GNU Emacs major mode for convenient plain text markup — and much more.

;;; Code:
(use-package org
  :ensure t
  :custom
  (org-ellipsis " ▾")
)

(use-package org-tempo
  :after org
)

(provide 'org-mode-rcp)
;;; Commentary:
;;
;;; org-mode-rcp.el ends here
