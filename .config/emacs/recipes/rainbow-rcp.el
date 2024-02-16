;;; rainbow-rcp.el --- Colorize color names and delimeters in buffers

;;; Code:
(eval-when-compile (require 'use-package))
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook
  (prog-mode    . rainbow-delimiters-mode)
  (conf-mode    . rainbow-delimiters-mode)
  (json-ts-mode . rainbow-delimiters-mode)
  (toml-ts-mode . rainbow-delimiters-mode)
)

(use-package rainbow-mode
  :ensure t
  :defer t
  :hook
  (prog-mode    . rainbow-mode)
  (conf-mode    . rainbow-mode)
  (json-ts-mode . rainbow-mode)
  (toml-ts-mode . rainbow-mode)
  (yaml-ts-mode . rainbow-mode)
)

(provide 'rainbow-rcp)

;;; Commentary:
;;
;;; rainbow-rcp.el ends here
