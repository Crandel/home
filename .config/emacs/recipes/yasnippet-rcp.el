;;; yasnippet-rcp.el --- Yet another snippet extension for Emacs

;;; Code:
(use-package yasnippet
  :ensure t
  :defer t
  :config
  (yas-reload-all)
  :hook
  (c++-mode              . yas-minor-mode)
  (c-mode                . yas-minor-mode)
  (emacs-lisp-mode       . yas-minor-mode)
  (fish-mode             . yas-minor-mode)
  (go-mode               . yas-minor-mode)
  (java-mode             . yas-minor-mode)
  (js-mode               . yas-minor-mode)
  (lisp-interaction-mode . yas-minor-mode)
  (markdown-mode         . yas-minor-mode)
  (python-mode           . yas-minor-mode)
  (rust-mode             . yas-minor-mode)
  (scala-mode            . yas-minor-mode)
  (terraform-mode        . yas-minor-mode)
  (web-mode              . yas-minor-mode)
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
