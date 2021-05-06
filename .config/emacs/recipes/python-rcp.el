;;; python-rcp.el --- Python support

;;; Code:
(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :commands (my-merge-imenu imenu-create-index-function)
  :init
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
  (:map python-mode-map
        ("RET" . newline-and-indent)
        ("M-RET" . newline)
        ("C-c C-b" . insert_pdb))
  )
(use-package pip-requirements
  :ensure t
  :defer t
)

(use-package py-isort
  :ensure t
  :after (python-mode)
  :defer t
)


(when (executable-find "autopep8")
  (use-package py-autopep8
    :ensure t
    :after (python-mode)
    :defer t
    )
  )

(when (executable-find "virtualenvwrapper.sh")
  (use-package pyvenv
    :ensure t
    :hook
    (python-mode)
    )
  )

(when (executable-find "pyright")
  (use-package lsp-pyright
    :ensure t
    :defer t
    :hook
    (python-mode . (lambda ()
                           (require 'lsp-pyright)
                           (lsp))))  ; or lsp-deferred
)

(provide 'python-rcp)

;;; Commentary:
;;

;;; python-rcp.el ends here
