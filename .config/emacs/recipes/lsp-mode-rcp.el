;;; lsp-mode-rcp.el --- Emacs client/library for the Language Server Protocol

;;; Code:
(use-package lsp-mode
  :ensure t
  :defer t
  :commands (lsp lsp-deferred)
  :custom
  (lsp-completion-provider                :none)
  (lsp-completion-show-detail             t)
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
  :bind(
  :map lsp-mode-map
  ([remap xref-find-definitions] . lsp-find-definition)
  ([remap xref-find-references] . lsp-find-references)
  )
  :hook ((rust-mode . (lambda()
                        (setq-default lsp-rust-server 'rust-analyzer
                              lsp-semantic-tokens-enable nil)
                        (lsp-deferred)
                        ))
         (c++-mode       . lsp-deferred)
         (java-mode      . lsp-deferred)
         (js2-mode       . lsp-deferred)
         (scala-mode     . lsp-deferred)
         (terraform-mode . lsp-deferred)
         (vimrc-mode     . lsp-deferred)
         (yaml-mode      . lsp-deferred)
         ;; (sh-mode        . lsp)
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
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        ("C-," . lsp-ui-peek-jump-backward)
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

(provide 'lsp-mode-rcp)
;;; Commentary:
;;
;;; lsp-mode-rcp.el ends here
