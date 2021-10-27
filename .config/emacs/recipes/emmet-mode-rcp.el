;;; emmet-mode-rcp.el --- Emmet's support for emacs

;;; Code:
(eval-when-compile (require 'use-package))
(use-package emmet-mode
  :ensure t
  :defer t
  :hook
  (sgml-mode)
  (html-mode)
  (web-mode)
  (css-mode)
)

(provide 'emmet-mode-rcp)
;;; Commentary:
;;
;;; emmet-mode-rcp.el ends here
