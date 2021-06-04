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
  (consult--project-root #'get-project-root)
  (consult-preview-key (kdb "M-."))
  :bind
  ("C-s" . consult-line-symbol-at-point)
  ("C-x g" . consult-ripgrep)
  ("C-x b" . consult-buffer)
  ("C-x C-b" . consult-buffer)
  ("C-p" . consult-recent-file)
  ([F10] . consult-imenu)
  :chords
  ("bl" . consult-buffer)
)


(provide 'consult-rcp)

;;; Commentary:
;;
;;; consult-rcp.el ends here
