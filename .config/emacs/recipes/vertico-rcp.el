;;; vertico-rcp.el --- Vertico provides a minimalistic vertical completion UI, which is based on the default completion system.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package vertico
  :ensure t
  :commands (vertico--format-candidate vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-buffer-display-action
   '(display-buffer-in-side-window
     (window-height . 13)
     (side . top)))
  :bind
  (:map vertico-map
        ([right] . vertico-insert)
        ([left]  . vertico-directory-up)
        ("M-x"  . vertico-quick-insert)
        ("C-q"  . vertico-quick-exit)
        )
  :init
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand)))
  (vertico-mode)
  (vertico-buffer-mode)
  (vertico-indexed-mode)
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history)
  :hook
  (minibuffer-setup . vertico-repeat-save)
)

(provide 'vertico-rcp)

;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; vertico-rcp.el ends here
