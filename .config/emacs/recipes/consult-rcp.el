;;; consult-rcp.el --- Consult provides various practical commands based on the Emacs completion function completing-read,
;;; which allows to quickly select an item from a list of candidates with completion.

;;; Code:
(use-package consult
  :ensure t
  :defer t
  :functions (get-project-root consult-line-symbol-at-point consult-ripgrep-symbol-at-point)
  :custom
  (consult-find-command "fd --color=never --full-path ARG OPTS")
  (consult-project-root-function #'get-project-root)
  (consult-narrow-key ",")
  :config
  (defun get-project-root ()
    (if (fboundp 'projectile-project-root)
      (projectile-project-root)
      (vc-root-dir)))
  (defun consult-line-symbol-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol)))
  (defun consult-ripgrep-symbol-at-point ()
    (interactive)
    (consult-ripgrep (get-project-root) (thing-at-point 'symbol)))
  (consult-customize
   consult--source-file
   consult-recent-file
   consult-ripgrep
   :preview-key '[M-.])
  :bind
  ("C-s" . consult-line-symbol-at-point)
  ("C-c s" . consult-multi-occur)
  ("C-x g" . consult-ripgrep-symbol-at-point)
  ("C-x C-g" . consult-ripgrep)
  ("C-x b" . consult-buffer)
  ("C-x C-b" . consult-buffer)
  ("C-p" . consult-buffer)
  ([f10] . consult-imenu)
  :chords
  ("bl" . consult-buffer)
)

(use-package consult-flycheck
  :ensure t
  :bind
  ("C-c n" . consult-flycheck)
)

(use-package consult-lsp
  :ensure t
  :after (consult lsp)
)

(provide 'consult-rcp)

;;; Commentary:
;;
;;; consult-rcp.el ends here
