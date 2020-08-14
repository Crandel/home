;;; emmet-mode-rcp.el --- Emmet's support for emacs

;;; Commentary:
;; 

;;; Code:

(use-package emmet-mode
  :ensure t
  :hook
  (sgml-mode)
  (html-mode)
  (web-mode)
  (css-mode)
)

(provide 'emmet-mode-rcp)

;;; emmet-mode-rcp.el ends here
