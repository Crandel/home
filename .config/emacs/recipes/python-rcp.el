;;; python-rcp.el --- Python support

;;; Code:
(eval-when-compile (require 'use-package))
(use-package python-ts-mode
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python" . python-ts-mode)
  :commands (my-merge-imenu imenu-create-index-function)
  :preface
  (defun insert_pdb ()
    (interactive)
    (progn
      (move-end-of-line nil)
      (newline-and-indent)
      (insert "import pdb; pdb.set_trace()")))
  (defun my-merge-imenu ()
    (interactive)
    (let ((mode-imenu (imenu-default-create-index-function))
          (custom-imenu (imenu--generic-function imenu-generic-expression)))
      (append mode-imenu custom-imenu)))
  :custom
  (python-shell-completion-native             nil)
  (python-shell-prompt-detect-failure-warning nil)
  (indent-tabs-mode                           nil)
  (tab-width                                  4)
  (python-indent                              4)
  (imenu-create-index-function                'my-merge-imenu)
  :bind
  (:map python-ts-mode-map
        ("RET" . reindent-then-newline-and-indent)
        ("M-RET" . newline)
        ("C-c C-b" . insert_pdb))
)


(use-package pip-requirements
  :ensure t
  :defer t
)
;; (use-package lsp-pyright
;;   :ensure t
;;   :ensure-system-package pyright
;;   :custom
;;   (lsp-pyright-disable-organize-imports nil)
;;   (lsp-pyright-auto-import-completions  t)
;;   (lsp-pyright-auto-search-paths        nil)
;;   :hook
;;   (python-ts-mode . lsp-deferred)
;; )

(provide 'python-rcp)

;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; python-rcp.el ends here
