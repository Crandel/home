;;; consult-rcp.el --- Consult provides various practical commands based on the Emacs completion function completing-read,
;;; which allows to quickly select an item from a list of candidates with completion.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package consult
  :ensure t
  :defer t
  :functions (get-project-root consult-line-symbol-at-point consult-ripgrep-symbol-at-point)
  :custom
  (consult-find-command "fd --color=never --full-path ARG OPTS")
  (consult-project-root-function #'get-project-root)
  (consult-narrow-key ",")
  :preface
  (defun consult-narrow-left ()
    (interactive)
    (when consult--narrow-keys
      (consult-narrow
       (if consult--narrow
           (let ((idx (seq-position consult--narrow-keys
                                    (assq consult--narrow consult--narrow-keys))))
             (unless (eq idx 0)
               (car (nth (1- idx) consult--narrow-keys))))
         (caar (last consult--narrow-keys))))))

  (defun consult-narrow-right ()
    (interactive)
    (when consult--narrow-keys
      (consult-narrow
       (if consult--narrow
           (let ((idx (seq-position consult--narrow-keys
                                    (assq consult--narrow consult--narrow-keys))))
             (unless (eq idx (1- (length consult--narrow-keys)))
               (car (nth (1+ idx) consult--narrow-keys))))
         (caar consult--narrow-keys)))))
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
  :config
  (consult-customize
   consult--source-hidden-buffer
   consult--source-buffer
   consult--source-recent-file
   consult--source-bookmark
   consult--source-project-buffer
   consult--source-project-recent-file
   :preview-key '[M-.])
  :bind
  ("C-s" . consult-line-symbol-at-point)
  ("C-c s" . consult-multi-occur)
  ("C-x g" . consult-ripgrep-symbol-at-point)
  ("C-x C-g" . consult-ripgrep)
  ("C-h C-m" . consult-minor-mode-menu)
  ("C-p" . consult-buffer)
  ([f10] . consult-imenu)
  :bind(:map consult-narrow-map
             ([C-right] .  consult-narrow-right)
             ([C-left] .  consult-narrow-left)
  )
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

(use-package consult-yasnippet
  :ensure t
  :after consult
)

(provide 'consult-rcp)

;;; Commentary:
;;
;;; consult-rcp.el ends here
