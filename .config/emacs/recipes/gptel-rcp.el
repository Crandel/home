;;; gptel-rcp.el --- A no-frills ChatGPT client for Emacs

;;; Code:
(eval-when-compile (require 'use-package))
(use-package gptel
  :ensure t
  :custom
  (gptel-default-mode 'org-mode)
)

(provide 'gptel-rcp)

;;; Commentary:
;;
;;; gptel-rcp.el ends here
