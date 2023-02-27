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
  (consult--regexp-compiler consult--orderless-regexp-compiler)
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
  (defun consult--orderless-regexp-compiler (input type &rest _config)
    (setq input (orderless-pattern-compiler input))
    (cons
     (mapcar (lambda (r) (consult--convert-regexp r type)) input)
     (lambda (str) (orderless--highlight input str))))
  :config
  (consult-customize
   consult--source-hidden-buffer
   consult--source-buffer
   consult--source-recent-file
   consult--source-bookmark
   consult--source-project-buffer
   consult--source-project-recent-file
   :preview-key "M-.")
  :hook
  (evil-leader-mode . (lambda ()
                        (evil-leader/set-key
                          "/" 'consult-ripgrep-symbol-at-point
                          "p" 'consult-buffer
                          "g" 'consult-ripgrep
                          "i" 'consult-imenu-multi
                        )
                       )
  )
  :bind (
  ("C-s"   . consult-line-symbol-at-point)
  ("C-c s" . consult-multi-occur)
  ("C-x g" . consult-ripgrep-symbol-at-point)
  ("C-x C-g" . consult-ripgrep)
  ("C-h C-m" . consult-minor-mode-menu)
  ("C-p" . consult-buffer)
  ([remap switch-to-buffer] . consult-buffer)
  ([remap yank-pop] . consult-yank-pop)
  ([remap goto-line] . consult-goto-line)
  ([f10] . consult-imenu)
  :map consult-narrow-map
  ([C-right] .  consult-narrow-right)
  ([C-left] .  consult-narrow-left)
  :map minibuffer-local-map
  ([remap previous-matching-history-element] . consult-history)
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
  :hook
  (evil-mode . (lambda ()
    (evil-global-set-key 'normal "grf" 'consult-lsp-file-symbols)
    (evil-global-set-key 'normal "grd" 'consult-lsp-symbols)
  ))
  :bind
  ("C-c f" . consult-lsp-file-symbols)
  ("C-c d" . consult-lsp-symbols)
)

;; (use-package consult-yasnippet
;;   :ensure t
;;   :after consult
;; )

(provide 'consult-rcp)

;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; consult-rcp.el ends here
