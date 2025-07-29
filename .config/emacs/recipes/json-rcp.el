;;; json-rcp.el --- json, toml and yaml modes

;;; Code:
(eval-when-compile (require 'use-package))
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
)

(use-package jsonian
  :ensure t
  :mode
  ("\\.json\\'"       . jsonian-mode)
  ("\\.jsonc\\'"      . jsonian-mode)
  ("\\.jsonc.tmpl\\'" . jsonian-mode)
  ("\\.vil\\'"        . jsonian-mode)
)

(use-package yaml-ts-mode
  :mode ("\\.yml\\'" "\\.yml.tmpl\\'")
)

(use-package toml-ts-mode
  :mode ("\\.toml\\'" "\\.toml.tmpl\\'")
)

(provide 'json-rcp)
;;; Commentary:
;;; json-rcp.el ends here
