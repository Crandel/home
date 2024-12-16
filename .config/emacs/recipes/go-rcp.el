;;; go-rcp.el --- Golang support

;;; Code:
(eval-when-compile (require 'use-package))
(use-package go-ts-mode
  :mode "\\.go\\'"
  :bind
  (:map go-ts-mode-map
    ("C-c i a" . treesit-beginning-of-defun)
    ("C-c i e" . treesit-end-of-defun)
    ("RET"     . reindent-then-newline-and-indent)
    ("M-RET"   . newline)
   )
  :custom
  (go-ts-mode-indent-offset 4)
  :config
  (add-to-list 'exec-path "~/.local/bin")
  (setq-default eglot-workspace-configuration
    '((:gopls .
        ((staticcheck . t)
         (symbolScope . "workspace")
         (analyses . (
                      (nilness . t)
                      (shadow . t)
                      (unusedwrite . t)
                      (fieldalignment . t)
                      (escape . t)
         ))
         (hints . ((parameterNames . t)))
         )
        ))
    )
)

(use-package go-tag
  :ensure t
  :bind
  (:map go-ts-mode-map
    ("C-c i r" . go-tag-remove)
    ("C-c i i" . go-tag-add)
    )
)

(use-package godoctor
  :ensure t
)

(use-package gotest
  :ensure t
  :after go-ts-mode
  :commands (go-test-current-file go-test-current-test)
  :bind
  (:map go-ts-mode-map
    ("C-c i t" . go-test-current-test)
    ("C-c i f" . go-test-current-file)
    )
)

(provide 'go-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; go-rcp.el ends here
