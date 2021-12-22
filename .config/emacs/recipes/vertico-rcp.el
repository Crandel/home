;;; vertico-rcp.el --- Vertico provides a minimalistic vertical completion UI, which is based on the default completion system.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package vertico
  :ensure t
  :commands (vertico--format-candidate vertico-mode)
  :custom
  (vertico-cycle t)
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
  (vertico-indexed-mode)
)

(use-package mini-popup
  :load-path "mini-popup"
  :if window-system
  :defer 1
  :preface
  (defun mini-popup-height-resize ()
    (* (1+ (min vertico--total vertico-count)) (default-line-height)))
  (defun mini-popup-height-fixed ()
    (* (1+ (if vertico--input vertico-count 0)) (default-line-height)))
  :custom
  (mini-popup--height-function #'mini-popup-height-fixed)
  :config
  (advice-add #'vertico--resize-window :around
              (lambda (&rest args)
                (unless mini-popup-mode
                  (apply args))))
  (mini-popup-mode 1)
  :hook
  (consult--completion-refresh-hook . (lambda() (&rest _) (mini-popup--setup) 99))
)

(provide 'vertico-rcp)

;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; vertico-rcp.el ends here
