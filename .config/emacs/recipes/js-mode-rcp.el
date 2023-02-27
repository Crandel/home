;;; js-mode-rcp.el --- Javascript, json and yaml modes

;;; Code:
(eval-when-compile (require 'use-package))
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
)

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

(provide 'js-mode-rcp)
;;; Commentary:
;;
;;; js-mode-rcp.el ends here
