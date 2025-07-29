;;; gptel-rcp.el --- A simple LLM client for Emacs.


;;; Code:
(eval-when-compile (require 'use-package))
(use-package gptel
  :ensure t
  :preface
  (defun vd/setup-ollama-gptel ()
    "Fill ollama models."
    (interactive)
    (gptel-make-ollama "Ollama"
      :host "0.0.0.0:11434"
      :stream t
      :models (vd/ollama-list-remote-models "http://0.0.0.0:11434")
      )
    (gptel-make-ollama "Ollama Remote"
      :host "192.168.178.31:11434"
      :stream t
      :models (vd/ollama-list-remote-models "http://192.168.178.31:11434")
      )
    )
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-prompt-prefix-alist nil)
  (gptel-org-branching-context t)
  :config
  (setq gptel--known-backends nil
        gptel-model 'gemini-2.0-flash-lite-preview-02-05
        gptel-expert-commands t)
  (gptel-make-gemini "Gemini"
    :key gptel-api-key
    :stream t
    )
  :bind
  ("C-c z" . gptel-menu)
)

(provide 'gptel-rcp)

;;; Commentary:
;;
;;; gptel-rcp.el ends here
