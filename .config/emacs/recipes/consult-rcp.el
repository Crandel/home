;;; consult-rcp.el --- Consult provides various practical commands based on the Emacs completion function completing-read,
;;; which allows to quickly select an item from a list of candidates with completion.

;;; Code:
(use-package consult
  :ensure t
  :init
  (defun get-project-root ()
    (if (fboundp 'projectile-project-root)
      (projectile-project-root)
      (vc-root-dir)))
  (defun consult-line-symbol-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol)))
  :custom
  (consult--project-root get-project-root)
  (consult-find-command "fd --color=never --full-path ARG OPTS")
  :config
  (consult-customize
   consult--source-file
   consult-recent-file
   :preview-key '[M-.])
  :bind
  ("C-s" . consult-line-symbol-at-point)
  ("C-c s" . consult-multi-occur)
  ("C-x g" . consult-ripgrep)
  ("C-x b" . consult-buffer)
  ("C-x C-b" . consult-buffer)
  ("C-p" . consult-recent-file)
  ([f10] . consult-imenu)
  :chords
  ("bl" . consult-buffer)
)

(use-package consult-flycheck
  :ensure t
  :after consult
)

(use-package consult-lsp
  :ensure t
  :after (consult lsp)
)

(provide 'consult-rcp)

;;; Commentary:
;;
;;; consult-rcp.el ends here
