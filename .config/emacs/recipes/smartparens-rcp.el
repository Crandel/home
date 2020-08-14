;;; smartparens-rcp.el --- Smart paren support

;;; Commentary:
;; 

;;; Code:

(use-package smartparens
  :ensure t
  :demand t
  :init
  (smartparens-global-mode)
  (show-smartparens-global-mode)
  (require 'smartparens-config)
  :bind (
  ("C-c w" . sp-rewrap-sexp)
  ("C-c r" . sp-unwrap-sexp)
  ("C-c f" . sp-forward-sexp)
  ("C-c d" . sp-backward-sexp))
  :chords (
  ("''" . sp-rewrap-sexp))
)

(provide 'smartparens-rcp)

;;; smartparens-rcp.el ends here
