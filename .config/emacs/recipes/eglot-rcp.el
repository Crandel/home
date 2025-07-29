;;; eglot-rcp.el --- Eglot is the Emacs client for the Language Server Protocol (LSP).

;;; Code:
(eval-when-compile (require 'use-package))
(use-package eglot
  :preface
  (defun vd/eglot-organize-imports-on-save ()
    (eglot-code-action-organize-imports (point-min) (point-max)))
  :custom
  (eglot-autoreconnect        t)
  (eglot-autoshutdown         t)
  (eglot-code-action-indications '(eldoc-hint nearby mode-line margin))
  (eglot-extend-to-xref       t)
  (eglot-events-buffer-config '(:size 0 :format lisp))
  (eglot-report-progress      t)
  :config
  (setq eglot-stay-out-of '(imenu))
  :bind (
  :map eglot-mode-map
  ("C-."     . eglot-find-implementation)
  ("C-c l r" . eglot-rename)
  ("C-c l f" . eglot-format)
  ("C-c l F" . eglot-format-buffer)
  ("C-c l a" . eglot-code-actions)
  ("C-c l o" . eglot-code-action-organize-imports)
  ("C-c l i" . eglot-code-action-inline)
  ("C-c l e" . eglot-code-action-extract)
  ("C-c l w" . eglot-code-action-rewrite)
  ("C-c l q" . eglot-code-action-quickfix)
  ("C-c l l" . eglot-list-connections)
  ("C-c l Q" . eglot-shutdown)
  ("C-c l S" . eglot-shutdown-all)
  ("C-c l R" . eglot-reconnect)
  ("C-c l C" . eglot-clear-status)
  ("C-c l g" . eglot-forget-pending-continuations)
  ("C-c l s" . eglot-show-workspace-configuration)
  ("C-c l L" . eglot-events-buffer)
  ("C-c l E" . eglot-stderr-buffer)
  )
  :hook ((c-ts-mode      . eglot-ensure)
         (c++-ts-mode    . eglot-ensure)
         (rust-ts-mode   . eglot-ensure)
         (java-mode      . eglot-ensure)
         (js-ts-mode     . eglot-ensure)
         (kotlin-mode    . eglot-ensure)
         (sql-mode       . eglot-ensure)
         ;; (scala-mode     . eglot-ensure)
         ;; (terraform-mode . eglot-ensure)
         ;; (vimrc-mode     . eglot-ensure)
         (toml-ts-mode   . eglot-ensure)
         (yaml-ts-mode   . eglot-ensure)
         (bash-ts-mode   . eglot-ensure)
         (go-ts-mode     . (lambda()
                             (add-hook 'before-save-hook #'eglot-format-buffer t t)
                             (add-hook 'before-save-hook #'vd/eglot-organize-imports-on-save t t)
                             (eglot-ensure)
                             ))
         )
)

(provide 'eglot-rcp)
;;; Commentary:
;;
;;; eglot-rcp.el ends here
