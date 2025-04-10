;;; go-rcp.el --- Golang support

;;; Code:
(eval-when-compile (require 'use-package))
(use-package go-ts-mode
  :mode "\\.go\\'"
  :bind
  (:map go-ts-mode-map
    ("C-c i a" . treesit-beginning-of-defun)
    ("C-c i e" . treesit-end-of-defun)
    ("C-c i t" . go-ts-mode-test-function-at-point)
    ("C-c i f" . go-ts-mode-test-this-file)
    ("C-c i p" . go-ts-mode-test-this-package)
    ("RET"     . reindent-then-newline-and-indent)
    ("M-RET"   . newline)
   )
  :custom
  (go-ts-mode-indent-offset 4)
  :config
  (add-to-list 'exec-path "~/.local/bin")
  (setq-default eglot-workspace-configuration
    '((:gopls .
        ((staticcheck     . t)
         (usePlaceholders . t)
         (local           . "github.com/talon-one/")
         (symbolScope     . "workspace")
         (analyses        . (
            (nilness . t)
            (shadow . t)
            (unusedwrite . t)
            (escape . t)
         ))
         (hints . ((parameterNames . t)))
         )
        ))
    )
)

(use-package crandel-go-tag
  :vc (:url "https://github.com/Crandel/emacs-go-tag"
       :rev :newest)
  :after go-ts-mode
  :bind
  (:map go-ts-mode-map
    ("C-c i r" . go-tag-remove)
    ("C-c i i" . go-tag-add)
    )
)

;; (use-package godoctor
;;   :ensure t
;; )

(provide 'go-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; go-rcp.el ends here
