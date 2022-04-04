;;; lsp-mode-rcp.el --- Emacs client/library for the Language Server Protocol

;;; Code:
(eval-when-compile (require 'use-package))
(use-package lsp-mode
  :ensure t
  :defer t
  :functions (lsp lsp-deferred lsp-completion-at-point)
  :custom
  (lsp-completion-provider                :none)
  (lsp-completion-show-detail             t)
  (lsp-completion-sort-initial-results    t)
  (lsp-completion-show-kind               t)
  (lsp-diagnostics-provider               :flycheck)
  (lsp-enable-completion-at-point         t)
  (lsp-enable-file-watchers               nil)
  (lsp-enable-imenu                       t)
  (lsp-enable-on-type-formatting          nil)
  (lsp-enable-snippet                     t)
  (lsp-enable-symbol-highlighting         t)
  (lsp-enable-text-document-color         t)
  (lsp-enable-which-key-integration       t)
  (lsp-enable-xref                        t)
  (lsp-file-watch-threshold               1000)
  (lsp-headerline-breadcrumb-enable       t)
  (lsp-headerline-breadcrumb-icons-enable t)
  (lsp-headerline-breadcrumb-segments     '(project file symbols))
  (lsp-idle-delay                         0.5)
  (lsp-imenu-container-name-separator     t)
  (lsp-imenu-show-container-name          t)
  (lsp-keymap-prefix                      "C-l")
  (lsp-lens-enable                        t)
  (lsp-log-io                             nil)
  (lsp-modeline-code-actions-enable       t)
  (lsp-modeline-code-actions-segments     '(count icon name))
  (lsp-modeline-diagnostics-enable        t)
  (lsp-signature-auto-activate            nil)
  (read-process-output-max                (* 1024 1024)) ;; 1mb
  :config
  (add-to-list 'lsp-language-id-configuration '(k8s-mode . "yaml"))
  :bind(
  :map lsp-mode-map
  ([remap xref-find-definitions] . lsp-find-definition)
  ([remap xref-find-references] . lsp-find-references)
  )
  :hook ((rust-mode . (lambda()
                        (setq-default lsp-rust-server                              'rust-analyzer
                                      lsp-rust-analyzer-server-display-inlay-hints t
                                      lsp-semantic-tokens-enable                   nil)
                        (lsp-deferred)
                        ))
         (c++-mode       . lsp-deferred)
         (java-mode      . lsp-deferred)
         (js2-mode       . lsp-deferred)
         (scala-mode     . lsp-deferred)
         (terraform-mode . lsp-deferred)
         (sql-mode       . lsp-deferred)
         ;; (vimrc-mode     . lsp-deferred)
         ;; (yaml-mode      . lsp-deferred)
         ;; (sh-mode        . lsp-deferred)
         )
)

(use-package lsp-ui
  :ensure t
  :defer t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-delay                   2)
  (lsp-ui-doc-max-height              3)
  (lsp-ui-doc-max-width               30)
  (lsp-ui-doc-show-with-cursor        nil)
  (lsp-ui-doc-show-with-mouse         t)
  (lsp-ui-peek-show-directory         t)
  (lsp-ui-sideline-enable             nil)
  (lsp-ui-sideline-show-code-actions  nil)
  (lsp-ui-sideline-show-hover         nil)
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-jump-backward)
        ("C-," . lsp-ui-peek-find-references)
        )
)

(use-package lsp-treemacs
  :ensure t
  :defer t
  :commands lsp-treemacs-errors-list
  :after treemacs
  :hook
  (treemacs-mode . lsp-treemacs-sync-mode)
)

(use-package dap-mode
  :ensure t
  :commands (dap-hydra go-dap-setup)
  :preface
  (defun go-dap-setup ()
    (interactive)
    (require 'dap-go)
    (dap-mode 1)
    (dap-ui-mode 1)
    (dap-tooltip-mode 1)
    (dap-ui-controls-mode 1)
  )
  :hook
  (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))

)

(provide 'lsp-mode-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; lsp-mode-rcp.el ends here
