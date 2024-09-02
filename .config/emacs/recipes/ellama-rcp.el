;;; ellama-rcp.el --- Ellama is a tool for interacting with large language models from Emacs.


;;; Code:
(eval-when-compile (require 'use-package))
(use-package ellama
  :ensure t
  :custom
  (ellama-always-show-chain-steps  t)
  (ellama-auto-scroll              t)
  (ellama-keymap-prefix            "C-c e")
  (ellama-language                 "Ukrainian")
  (ellama-spinner-type             'moon)
  :config
  (setq llama-prv (make-llm-ollama
                   :chat-model "llama3.1:latest"
                   :embedding-model "llama3.1:latest")
        llama-pro-prv (make-llm-ollama
                       :chat-model "llama3.1:70b"
                       :embedding-model "llama3.1:70b")
        codellama-prv (make-llm-ollama
                       :chat-model "codellama:latest"
                       :embedding-model "codellama:latest")
        codestral-prv (make-llm-ollama
                          :chat-model "codestral:latest"
                          :embedding-model "codestral:latest"))
  (setopt ellama-provider llama-prv)
  (setopt ellama-naming-provider llama-prv)
  (setopt ellama-providers
          '(("codellama"    . codellama-prv)
            ("llama"        . llama-prv)
            ("llama-pro"    . llama-pro-prv)
            ("codestral"    . codestral-prv)))
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
