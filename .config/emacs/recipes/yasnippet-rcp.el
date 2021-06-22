;;; yasnippet-rcp.el --- Yet another snippet extension for Emacs

;;; Code:
(use-package yasnippet
  :ensure t
  :defer t
  :commands (yas-minor-mode my-yas-try-expanding)
  :preface
  (defun my-yas-try-expanding ()
    (interactive)
    (when yas-minor-mode (yas-expand)))
  :custom
  (yas-snippet-dirs '("~/.config/emacs/snippets"))
  :config
  (yas-reload-all)
  :hook
  (prog-mode . yas-minor-mode)
  :bind
  ("C-<tab>" . my-yas-try-expanding)
)

(use-package yafolding
  :ensure t
  :defer t
)

(use-package yasnippet-snippets
  :ensure t
  :defer t
)

(provide 'yasnippet-rcp)
;;; Commentary:
;;
;;; yasnippet-rcp.el ends here
