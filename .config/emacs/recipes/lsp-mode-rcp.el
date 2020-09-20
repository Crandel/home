;;; lsp-mode-rcp.el --- Emacs client/library for the Language Server Protocol

;;; Commentary:
;; 

;;; Code:

(use-package lsp-mode
  :ensure t
  :custom
  (lsp-enable-completion-at-point     t)
  (lsp-enable-imenu                   t)
  (lsp-enable-semantic-highlighting   t)
  (lsp-enable-text-document-color     t)
  (lsp-enable-xref                    t)
  (lsp-idle-delay                     0.500)
  (lsp-imenu-container-name-separator t)
  (lsp-imenu-show-container-name      t)
  (lsp-keymap-prefix                  "C-l")
  (lsp-prefer-capf                    t)
  (lsp-signature-auto-activate        nil)
  (lsp-enable-which-key-integration   t)
  (gc-cons-threshold                  100000000)
  (read-process-output-max            (* 1024 1024)) ;; 1mb
  :hook ((lsp-mode . lsp-lens-mode)
         (rust-mode . (lambda()
                            (setq lsp-rust-server 'rust-analyzer
                                  lsp-enable-semantic-highlighting nil)
                            (lsp)
                            ))
         (c++-mode . lsp)
         (java-mode . lsp)
         (scala-mode . lsp)
         (vimrc-mode . lsp)
         (js2-mode . lsp)
         ;; (sh-mode . lsp)

         ;; (yaml-mode . lsp)
         )
)

(use-package lsp-ui
  :ensure t
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
  :config
  (lsp-treemacs-sync-mode 1)
)

(provide 'lsp-mode-rcp)

;;; lsp-mode-rcp.el ends here
