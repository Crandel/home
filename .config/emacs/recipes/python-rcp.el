;;; python-rcp.el --- Python support

;;; Code:
(use-package python-mode
  :ensure t
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
  (defvar my/python-mode-map
    (let ((map (make-keymap)))
     (define-key map (kbd "p") 'run-python)
     (define-key map (kbd "s") 'python-shell-send-statement)
     (define-key map (kbd "f") 'python-shell-send-file)
     (define-key map (kbd "r") 'python-shell-send-region)
     (define-key map (kbd "b") 'python-shell-send-buffer)
      map)
    "Custom keymap for python major mode")
  :custom
  (python-shell-completion-native             nil)
  (python-shell-prompt-detect-failure-warning nil)
  (indent-tabs-mode                           nil)
  (tab-width                                  4)
  (python-indent                              4)
  (imenu-create-index-function                'my-merge-imenu)
  :hook
  (evil-mode . (lambda()
                   (interactive)
                   (evil-define-key* '(normal visual) python-mode-map
                     (kbd "gf") my/python-mode-map)
                   ))
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
  :after python-mode
  :defer t
)


(when (executable-find "autopep8")
  (use-package py-autopep8
    :ensure t
    :after python-mode
    :defer t
    )
  )

(when (executable-find "black")
  (use-package python-black
    :ensure t
    :after python-mode
    :hook (python-mode . python-black-on-save-mode-enable-dwim)
    )
)

(when (executable-find "pyenv")
  (use-package pyvenv
    :ensure t
    :after python-mode
    :init
    (setenv "WORKON_HOME" "~/.pyenv/versions")
    :preface
    (defun pyvenv-autoload ()
      (interactive)
      "auto activate venv directory if exists"
      (unless pyvenv-virtual-env-name
        (message "Activate .python-version")
        (f-traverse-upwards (lambda (path)
                              (let ((venv-path (f-expand ".python-version" path)))
                                (when (f-exists? venv-path)
                                  (pyvenv-activate venv-path)))))))
    :hook
    (python-mode . pyvenv-mode)
    (pyvenv-mode . pyvenv-autoload)
    )

    (when (executable-find "pyright")
      (use-package lsp-pyright
        :ensure t
        :defer t
        :after pyvenv
        :custom
        (lsp-pyright-disable-organize-imports t)
        (lsp-pyright-auto-import-completions  t)
        (lsp-pyright-auto-search-paths        nil)
        :hook
        (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))  ; or lsp
      )
    )

(provide 'python-rcp)

;;; Commentary:
;;

;;; python-rcp.el ends here
