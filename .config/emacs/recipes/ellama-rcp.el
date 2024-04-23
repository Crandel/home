;;; ellama-rcp.el --- Ellama is a tool for interacting with large language models from Emacs.


;;; Code:
(eval-when-compile (require 'use-package))
(use-package ellama
  :ensure t
  :custom
  (ellama-keymap-prefix "C-c e")
  (ellama-language "Ukrainian")
  :config
  (setq llama-prv (make-llm-ollama
                   :chat-model "llama3"
                   :embedding-model "llama3:latest"))
  (setq gemma-prv (make-llm-ollama
                   :chat-model "gemma"
                   :embedding-model "gemma:latest"))
  (setq codegemma-prv (make-llm-ollama
                       :chat-model "codegemma"
                       :embedding-model "codegemma:latest"))
  (setq dolphincoder-prv (make-llm-ollama
                          :chat-model "dolphincoder"
                          :embedding-model "dolphincoder:latest"))
  (setopt ellama-provider llama-prv)
  (setopt ellama-naming-provider gemma-prv)
  (setopt ellama-providers
          '(("gemma"        . gemma-prv)
            ("codegemma"    . codegemma-prv)
            ("llama"        . llama-prv)
            ("dolphincoder" . dolphincoder-prv)))
  ;; Naming new sessions with llm
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  ;; Translation llm provider
  (setopt ellama-translation-provider (make-llm-ollama
                                       :chat-model "thinkverse/towerinstruct"
                                       :embedding-model "thinkverse/towerinstruct:latest"))
)
(provide 'ellama-rcp)

;;; Commentary:
;;;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; ellama-rcp.el ends here
