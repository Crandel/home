;;; json-rcp.el --- json, toml and yaml modes

;;; Code:
(eval-when-compile (require 'use-package))

(use-package json-ts-mode
  :mode
  ("\\.json\\'"       . json-ts-mode)
  ("\\.jsonc\\'"      . json-ts-mode)
  ("\\.jsonc.tmpl\\'" . json-ts-mode)
  ("\\.vil\\'"        . json-ts-mode)
)

(use-package jsonian
  :ensure t
  :config
  (setq-default eglot-workspace-configuration
                '(:yaml (:format (:enable t)
                         :validate t
                         :hover t
                         :completion t
                         :schemaStore (:enable t))
                  :json (:format (:enable t)
                         :validate (:enable t))
                  )
                )
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
