;;; lsp-mode-rcp.el --- Emacs client/library for the Language Server Protocol

;;; Code:
(use-package lsp-mode
  :ensure t
  :defer t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-completion-provider           :none)
  :custom
  (lsp-enable-completion-at-point         t)
  (lsp-enable-imenu                       t)
  (lsp-enable-semantic-highlighting       t)
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
  (lsp-log-io                             nil)
  (lsp-keymap-prefix                      "C-l")
  (lsp-modeline-code-actions-segments     '(count icon name))
  (lsp-prefer-capf                        t)
  (lsp-signature-auto-activate            nil)
  (read-process-output-max                (* 1024 1024)) ;; 1mb
  :bind(
  :map lsp-mode-map
  ([remap xref-find-definitions] . lsp-find-definition)
  ([remap xref-find-references] . lsp-find-references)
  )
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (rust-mode . (lambda()
                        (setq-default lsp-rust-server 'rust-analyzer
                              lsp-enable-semantic-highlighting nil)
                        (lsp-deferred)
                        ))
         (c++-mode       . lsp-deferred)
         (java-mode      . lsp-deferred)
         (js2-mode       . lsp-deferred)
         (scala-mode     . lsp-deferred)
         (terraform-mode . lsp-deferred)
         (vimrc-mode     . lsp-deferred)
         ;; (yaml-mode      . lsp)
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
  (lsp-ui-peek-show-directory         t)
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
