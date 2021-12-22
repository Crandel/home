;;; python-rcp.el --- Python support

;;; Code:
(eval-when-compile (require 'use-package))
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

(use-package py-autopep8
  :ensure t
  :ensure-system-package autopep8
  :after python-mode
  :defer t
)

(use-package python-black
  :ensure t
  :ensure-system-package black
  :hook (python-mode . python-black-on-save-mode-enable-dwim)
)
(use-package pyvenv
  :ensure t
  :commands pyvenv-autoload
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions")
  :config
  (defun pyvenv-autoload ()
    "auto activate venv directory if exists"
    (interactive)
    (unless pyvenv-virtual-env-name
      (message "Activate .python-version")
      (f-traverse-upwards (lambda (path)
                            (let ((venv-path (f-expand ".python-version" path)))
                              (when (f-exists? venv-path)
                                (pyvenv-workon (with-temp-buffer
                                                 (insert-file-contents venv-path)
                                                 (string-trim(buffer-string))))))))))
  (setq pyvenv-post-activate-hooks '(lambda ()
                                      (require'lsp-pyright)
                                      (lsp-deferred)))  ; or lsp

  :hook
  (python-mode . pyvenv-autoload)
)
(use-package lsp-pyright
  :ensure t
  :ensure-system-package pyright
  :custom
  (lsp-pyright-disable-organize-imports nil)
  (lsp-pyright-auto-import-completions  t)
  (lsp-pyright-auto-search-paths        nil)
)

(provide 'python-rcp)

;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; python-rcp.el ends here
