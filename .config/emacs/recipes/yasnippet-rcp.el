;;; yasnippet-rcp.el --- Yet another snippet extension for Emacs

;;; Code:
(eval-when-compile (require 'use-package))
(use-package yasnippet
  :ensure t
  :defer t
  :commands (yas-minor-mode vd/yas-try-expanding)
  :preface
  (defun vd/yas-try-expanding ()
    (interactive)
    (when yas-minor-mode (yas-expand)))
  :custom
  (yas-snippet-dirs '("~/.config/emacs/snippets"))
  :config
  (yas-reload-all)
  ;; :hook
  ;; (prog-mode . yas-minor-mode)
  :bind
  ("C-<tab>" . vd/yas-try-expanding)
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
