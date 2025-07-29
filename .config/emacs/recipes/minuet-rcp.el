;;; minuet-rcp.el --- Dance with LLM in Your Code. Minuet offers code completion as-you-type from popular LLMs including OpenAI, Gemini, Claude, Ollama, Llama.cpp, Codestral, and more.

;;; Code:

(use-package minuet
  :ensure t
  :bind
  (("M-i" . #'minuet-show-suggestion) ;; use overlay for completion
   ("C-c m" . #'minuet-configure-provider)
   :map minuet-active-mode-map
   ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
   ("M-k" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
   ("M-j" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
   ("M-a" . #'minuet-accept-suggestion) ;; accept whole completion
   ;; Accept the first line of completion, or N lines with a numeric-prefix:
   ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
   ("C-a" . #'minuet-accept-suggestion-line)
   ("M-g" . #'minuet-dismiss-suggestion))
  :init
  ;; if you want to enable auto suggestion.
  ;; Note that you can manually invoke completions without enable minuet-auto-suggestion-mode
  (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)

  :config
  ;; You can use M-x minuet-configure-provider to interactively configure provider and model
  (setq minuet-provider 'openai-fim-compatible
        minuet-n-completions 1
        minuet-context-window 512)
  (plist-put minuet-openai-fim-compatible-options :end-point "http://localhost:11434/v1/completions")
  (plist-put minuet-openai-fim-compatible-options :name "Ollama")
  (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
  (plist-put minuet-openai-fim-compatible-options :model "gemma3.4:local")
  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 64)
)

(provide 'minuet-rcp)

;;; Commentary:
;;
;;; minuet-rcp.el ends here
