;;; smartparens-rcp.el --- Smart paren support

;;; Code:
(use-package smartparens
  :ensure t
  :defer 1
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
;;; Commentary:
;;
;;; smartparens-rcp.el ends here
