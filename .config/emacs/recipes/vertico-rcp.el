;;; vertico-rcp.el --- Vertico provides a minimalistic vertical completion UI, which is based on the default completion system.

;;; Code:
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :bind
  (:map vertico-map
        ([right] . vertico-insert)
        ([left] . backward-kill-word)
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
)

(use-package vertico-directory
  :load-path "vertico/extensions"
  :bind
  (:map vertico-map
        ([left] . vertico-directory-up)
        )
  )

(provide 'vertico-rcp)

;;; Commentary:
;;
;;; vertico-rcp.el ends here
