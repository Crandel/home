;;; gptel-rcp.el --- A no-frills ChatGPT client for Emacs

;;; Code:
(eval-when-compile (require 'use-package))
(use-package gptel
  :ensure t
  :defer 1
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-model "dolphincoder:latest")
  :init
  (setq-default
   gptel-backend (gptel-make-ollama "Ollama"
                   :host "localhost:11434"
                   :stream t
                   :models '("dolphincoder:latest")))
)

(provide 'gptel-rcp)

;;; Commentary:
;;;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; gptel-rcp.el ends here
