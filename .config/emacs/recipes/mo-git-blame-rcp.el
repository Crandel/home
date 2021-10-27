;;; mo-git-blame-rcp.el --- Blame git changes in buffer

;;; Code:
(eval-when-compile (require 'use-package))
(use-package mo-git-blame
  :ensure t
  :bind
  ("C-c g" . mo-git-blame-current)
)

(provide 'mo-git-blame-rcp)

;;; Commentary:
;;
;;; mo-git-blame-rcp.el ends here
