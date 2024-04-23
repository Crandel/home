;;; json-mode-rcp.el --- Json mode

;;; Code:
(eval-when-compile (require 'use-package))
(use-package json-mode
  :ensure t
  :defer t
)

(use-package yaml-mode
  :ensure t
  :defer t
)

(use-package json-navigator
  :ensure t
  :defer t
)

(provide 'json-mode-rcp)
;;; Commentary:
;;
;;; json-mode-rcp.el ends here
