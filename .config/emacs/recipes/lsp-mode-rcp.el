;;; lsp-mode-rcp.el --- Emacs client/library for the Language Server Protocol

;;; Code:
(use-package lsp-mode
  :ensure t
  :defer t
  :custom
  (lsp-enable-completion-at-point     t)
  (lsp-enable-imenu                   t)
  (lsp-enable-semantic-highlighting   t)
  (lsp-enable-text-document-color     t)
  (lsp-enable-which-key-integration   t)
  (lsp-enable-xref                    t)
  (lsp-file-watch-threshold           10000)
  (lsp-idle-delay                     0.500)
  (lsp-imenu-container-name-separator t)
  (lsp-imenu-show-container-name      t)
  (lsp-keymap-prefix                  "C-l")
  (lsp-prefer-capf                    t)
  (lsp-signature-auto-activate        nil)
  (lsp-yaml-schema-store-local-db     "~/.config/emacs/.cache/lsp/lsp-yaml-schemas.json")
  (read-process-output-max            (* 1024 1024)) ;; 1mb
  :hook ((rust-mode . (lambda()
                            (setq lsp-rust-server 'rust-analyzer
                                  lsp-enable-semantic-highlighting nil)
                            (lsp)
                            ))
         (c++-mode       . lsp)
         (java-mode      . lsp)
         (js2-mode       . lsp)
         (scala-mode     . lsp)
         (terraform-mode . lsp)
         (vimrc-mode     . lsp)
         ;; (yaml-mode      . lsp)
         ;; (sh-mode        . lsp)
         )
)

(use-package lsp-lens
  :defer t
  :hook
  (lsp-mode . lsp-lens-mode))

(use-package lsp-ui
  :ensure t
  :defer t
  :custom
  (lsp-ui-doc-delay                   2)
  (lsp-ui-doc-max-height              3)
  (lsp-ui-doc-max-width               30)
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        )
)

(use-package lsp-treemacs
  :ensure t
  :defer t
  :after treemacs
  :hook
  (treemacs-mode . lsp-treemacs-sync-mode)
)

(provide 'lsp-mode-rcp)
;;; Commentary:
;;
;;; lsp-mode-rcp.el ends here
