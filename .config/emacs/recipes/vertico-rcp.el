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
  ("C-c r" . vertico-repeat)
  (:map vertico-map
        ([right] . vertico-insert)
        ([left]  . vertico-directory-up)
        ("C-f"  . vertico-quick-insert)
        ("C-q"  . vertico-quick-exit))
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
  (vertico-buffer-mode)
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history)
  :hook
  (minibuffer-setup . vertico-repeat-save)
  (evil-mode . (lambda ()
    (evil-global-set-key 'normal "g." 'vertico-repeat)))
  (evil-leader-mode . (lambda ()
    (evil-leader/set-key "." 'vertico-repeat)))
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
