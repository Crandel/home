;;; ellama-rcp.el --- Ellama is a tool for interacting with large language models from Emacs.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package ellama
  :ensure t
  :preface
  (setq-default vd/ellama-default-model     "llama3.2:local")
  (setq-default vd/ellama-codding-model     "qwen2.5-coder:local")
  (setq-default vd/ellama-embedding-model   "snowflake-arctic-embed2:latest")
  (setq-default vd/ellama-translation-model "aya-expanse:latest")
  ;; Convenience functions for mode change events
  (defun vd/ellama-select-code-completion ()
    (setq ellama-provider (alist-get vd/ellama-codding-model
                     ellama-providers nil nil 'string-equal)))
  (defun vd/ellama-select-chat ()
    (setq ellama-provider (alist-get vd/ellama-default-model
                     ellama-providers nil nil 'string-equal)))
  (defun vd/ollama-list-installed-models ()
    "Return the installed models"
    (let* ((ret (shell-command-to-string "ollama list"))
           (models (cdr (string-lines ret))))
      (if (and (string-match-p "NAME[[:space:]]*ID[[:space:]]*SIZE[[:space:]]*MODIFIED" ret) (length> models 0))
          (mapcar (lambda (m) (car (string-split m))) models)
        (message "Cannot detect installed models, please make sure Ollama server is started"))))
  (defun vd/ollama-set-providers ()
    (interactive)
    (setopt ellama-providers
            (cl-loop for model in (vd/ollama-list-installed-models)
                     collect (cons model (make-llm-ollama :chat-model model :embedding-model vd/ellama-embedding-model))))
    (setopt ellama-naming-provider (alist-get vd/ellama-default-model ellama-providers
                                                    nil nil 'string-equal))
    (setopt ellama-translation-provider (alist-get vd/ellama-translation-model ellama-providers
                                                    nil nil 'string-equal))
    (vd/ellama-select-chat)
    (add-hook 'prog-mode-hook  #'vd/ellama-select-code-completion)
    (add-hook 'text-mode-hook  #'vd/ellama-select-chat)
    )
  :custom
  (ellama-always-show-chain-steps  t)
  (ellama-auto-scroll              t)
  (ellama-fill-paragraphs         '(text-mode org-mode))
  (ellama-language                 "Russian")
  (ellama-spinner-type             'moon)
  (ellama-user-nick                "Crandel")
  (ellama-naming-scheme            'ellama-generate-name-by-llm)
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
