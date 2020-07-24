;;; python-rcp.el --- Python support

;;; Commentary:
;; 

;;; Code:

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (defun insert_ipdb ()
    (interactive)
    (progn
      (move-end-of-line nil)
      (newline-and-indent)
      (insert "import ipdb; ipdb.set_trace()")))
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
  (imenu-create-index-function 'my-merge-imenu)
  :bind
  (:map python-mode-map
        ("RET" . newline-and-indent)
        ("M-RET" . newline)
        ("C-c C-b" . insert_ipdb))
  )
(use-package pip-requirements :ensure t)

(use-package py-isort :ensure t)

(provide 'python-rcp)

;;; python-rcp.el ends here
