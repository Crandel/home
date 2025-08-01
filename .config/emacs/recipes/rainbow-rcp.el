;;; rainbow-rcp.el --- Colorize color names and delimeters in buffers.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook
  (prog-mode    . rainbow-delimiters-mode)
  (conf-mode    . rainbow-delimiters-mode)
)

(use-package colorful-mode
  :ensure t
  :defer t
  :hook
  (prog-mode    . colorful-mode)
  (conf-mode    . colorful-mode)
)

(provide 'rainbow-rcp)

;;; Commentary:
;;
;;; rainbow-rcp.el ends here
