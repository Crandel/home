;;; ellama-rcp.el --- Ellama is a tool for interacting with large language models from Emacs.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package ellama
  :ensure t
  :preface
  (setq-default vd/ellama-default-model       "gemma3:local")
  (setq-default vd/ellama-coding-model        "code.assistant:qwen")
  (setq-default vd/ellama-summarization-model "gemma3:local")
  (setq-default vd/ellama-extraction-model    "gemma3:local")
  (setq-default vd/ellama-embedding-model     "snowflake-arctic-embed2:latest")
  (setq-default vd/ellama-naming-model        "gemma3:local")
  (setq-default vd/ellama-translation-model   "russian:qwen")
  ;; Convenience functions for mode change events
  (defun vd/ollama-set-providers ()
    (interactive)
    (require 'llm-ollama)
    (setopt ellama-providers
            (cl-loop for model in (vd/ollama-list-installed-models)
                     collect (cons model (make-llm-ollama :chat-model model :embedding-model vd/ellama-embedding-model))))
    (setopt ellama-provider        (alist-get vd/ellama-default-model ellama-providers
                                                    nil nil 'string-equal))
    (setopt ellama-coding-provider        (alist-get vd/ellama-coding-model ellama-providers
                                                    nil nil 'string-equal))
    (setopt ellama-naming-provider        (alist-get vd/ellama-naming-model ellama-providers
                                                    nil nil 'string-equal))
    (setopt ellama-summarization-provider (alist-get vd/ellama-summarization-model ellama-providers
                                                    nil nil 'string-equal))
    (setopt ellama-extraction-provider    (alist-get vd/ellama-extraction-model ellama-providers
                                                    nil nil 'string-equal))
    (setopt ellama-translation-provider   (alist-get vd/ellama-translation-model ellama-providers
                                                    nil nil 'string-equal))
    )
  :custom
  (ellama-always-show-chain-steps  t)
  (ellama-auto-scroll              t)
  (ellama-fill-paragraphs         '(text-mode org-mode))
  (ellama-language                 "Russian")
  (ellama-spinner-type             'moon)
  (ellama-user-nick                "Crandel")
  (ellama-naming-scheme            'ellama-generate-name-by-words)
  :config
  (vd/ollama-set-providers)
  :bind
  ("C-c e" . ellama-transient-main-menu)
)

(provide 'ellama-rcp)

;;; Commentary:
;;;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; ellama-rcp.el ends here
