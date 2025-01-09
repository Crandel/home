;;; vertico-rcp.el --- Vertico provides a minimalistic vertical completion UI, which is based on the default completion system.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package vertico
  :ensure t
  :commands (vertico--format-candidate vertico-mode vertico-repeat)
  :preface
  ;; function to sort directories first
  (defun vd/sort-directories-first (files)
    ;; Still sort by history position, length and alphabetically
    (setq files (vertico-sort-history-length-alpha files))
    ;; But then move directories first
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))
  (defun vd/vertico-truncate-candidates (args)
    (if-let ((arg (car args))
             (type (get-text-property 0 'multi-category arg))
             ((eq (car-safe type) 'file))
             (w (max 60 (- (window-width) 38)))
             (l (length arg))
             ((> l w)))
        (setcar args (concat "…" (truncate-string-to-width arg l (- l w)))))
    args)
  :custom
  (vertico-cycle t)
  (vertico-buffer-display-action
   '(display-buffer-in-side-window
     (window-height . 13)
     (side . top)))
  :bind
  ("C-c s r" . vertico-repeat)
  (:map vertico-map
        ([right] . vertico-insert)
        ([left]  . vertico-directory-up)
        ("C-f"  . vertico-quick-insert)
        ("C-q"  . vertico-quick-exit))
  :init
  (defvar vd/vertico-transform-functions nil)
  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((not vd/vertico-transform-functions) null))
    (dolist (fun (ensure-list vd/vertico-transform-functions))
      (setq cand (funcall fun cand)))
    (cl-call-next-method cand prefix suffix index start))
  (vertico-mode)
  (vertico-indexed-mode)
  (vertico-buffer-mode)
  (vertico-multiform-mode)
  ;; add-to-list works if 'file isn't already in the alist
  ;; setq can be used but will overwrite all existing values
  (add-to-list 'vertico-multiform-categories
               '(file
                 ;; this is also defined in the wiki, uncomment if used
                 (vertico-sort-function . vd/sort-directories-first)
                 (vd/vertico-transform-functions . vd/highlight-directory)))
  (add-to-list 'vertico-multiform-commands
               '(execute-extended-command
                 (vd/vertico-transform-functions . vd/highlight-enabled-mode)))
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history)
  (advice-add #'vertico--format-candidate :filter-args #'vd/vertico-truncate-candidates)
  (advice-add #'vertico-insert :after #'vd/minibuffer-history)
  :hook
  (minibuffer-setup . vertico-repeat-save)
)

;; (use-package vertico-posframe
;;   :ensure t
;;   :after vertico
;;   :custom
;;   (vertico-posframe-poshandler    #'posframe-poshandler-frame-top-center)
;;   (vertico-posframe-fallback-mode vertico-buffer-mode)
;;   :config
;;   (vertico-posframe-mode)
;; )

(provide 'vertico-rcp)

;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; vertico-rcp.el ends here
