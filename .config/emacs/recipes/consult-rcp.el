;;; consult-rcp.el --- Consult provides various practical commands based on the Emacs completion function completing-read,
;;; which allows to quickly select an item from a list of candidates with completion.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package consult
  :ensure t
  :commands (consult-ripgrep consult-multi-occur consult-buffer consult-imenu consult-yank-from-kill-ring)
  :preface
  (defun get-project-root ()
    (if (fboundp 'projectile-project-root)
        (projectile-project-root)
      (vc-root-dir)))
  (defun consult--orderless-regexp-compiler (input type &rest _config)
    (setq input (orderless-pattern-compiler input))
    (cons
     (mapcar (lambda (r) (consult--convert-regexp r type)) input)
     (lambda (str) (orderless--highlight input str))))
  :custom
  (consult-find-command "fd --color=never --full-path ARG OPTS")
  (consult-project-function (lambda (_)
    (if (fboundp 'projectile-project-root)
        (projectile-project-root)
      (vc-root-dir))))
  (consult-narrow-key ",")
  (consult--regexp-compiler consult--orderless-regexp-compiler)
  ;; (completion-in-region-function 'consult-completion-in-region)
  (xref-show-xrefs-function       'consult-xref)
  (xref-show-definitions-function 'consult-xref)
  :config
  (consult-customize
   consult-buffer
   :preview-key "M-."
   consult-line
   :prompt "Search: "
   :add-history (seq-some #'thing-at-point '(region symbol))
   :initial (thing-at-point 'symbol)
   consult-line-multi
   :prompt "Search in buffers: "
   :add-history (seq-some #'thing-at-point '(region symbol))
   :initial (thing-at-point 'symbol)
   consult-focus-lines
   :prompt "Focus: "
   :add-history (seq-some #'thing-at-point '(region symbol))
   :initial (thing-at-point 'symbol)
   consult-ripgrep
   :add-history (seq-some #'thing-at-point '(region symbol))
   :initial (thing-at-point 'symbol))
  :bind (
  ("C-s"     . consult-line)
  ("C-c s i" . consult-imenu)
  ("C-c s l" . consult-line)
  ("C-c s f" . consult-focus-lines)
  ("C-c s s" . consult-line-multi)
  ("C-c s /" . consult-ripgrep)
  ("C-c s g" . consult-ripgrep)
  ("C-c s p" . consult-project-buffer)
  ("C-c s m" . consult-minor-mode-menu)
  ("C-h C-m" . consult-minor-mode-menu)
  ([remap list-buffers] . consult-buffer)
  ([remap yank-pop]     . consult-yank-pop)
  ([remap goto-line]    . consult-goto-line)
  ([f10] . consult-imenu)
  :map minibuffer-local-map
  ([remap previous-matching-history-element] . consult-history)
  )
)

;; (use-package consult-flycheck
;;   :ensure t
;;   :bind
;;   ("C-c s n" . consult-flycheck)
;; )

(use-package consult-eglot
  :ensure t
  :commands (consult-eglot-file-symbols consult-eglot-symbols)
  :after (consult eglot)
  :bind
  ("C-c s ," . consult-eglot-file-symbols)
  ("C-c s ." . consult-eglot-symbols)
)

(use-package consult-dir
  :ensure t
  :commands (consult-dir consult-dir-jump-file)
  :bind (("C-c s d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-d" . consult-dir)
         ("C-j" . consult-dir-jump-file))
)

(provide 'consult-rcp)

;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; consult-rcp.el ends here
