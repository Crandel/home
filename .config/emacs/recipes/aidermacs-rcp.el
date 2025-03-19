;;; aidermacs-rcp.el --- Emacs AI Pair Programming Solution with Aider.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package aidermacs
  :bind (("C-c t a" . aidermacs-transient-menu))
  :config
  (setenv "OLLAMA_API_BASE" "http://127.0.0.1:11434")
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-architect-model "ollama/code-assistant:local")
  (aidermacs-default-model "ollama/code-assistant:local")
)

(provide 'aidermacs-rcp)
;;; Commentary:
;;
;;; aidermacs-rcp.el ends here
