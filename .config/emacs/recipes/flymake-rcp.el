;;; flymake-rcp.el --- Continuous syntax checking for Emacs.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package flymake
  :bind
  ("C-c n n" . flymake-goto-next-error)
  ("C-c n p" . flymake-goto-prev-error)
  :custom-face
  (flymake-end-of-line-diagnostics-face ((t (:underline (:color "dark orange"
                                                         :style dots
                                                         :position t) :height 0.85))))
  :custom
  (flymake-indicator-type                 'fringes)
  (flymake-mode-line-lighter              "Flmk")
  (flymake-show-diagnostics-at-end-of-line 'short)
  (flymake-margin-indicators-string
   '((error   "!" compilation-error)
     (warning "?" compilation-warning)
     (note    "#" compilation-info)))
  (flymake-diagnostic-format-alist
   '((:help-echo origin code oneliner)
     (:eldoc origin code message)
     (:eldoc-echo origin code oneliner)
     (t origin code oneliner)))
  :config
  (flymake-mode t)
  :hook
  (prog-mode . flymake-mode)
  (text-mode . flymake-mode)
  (web-mode  . flymake-mode)
)

(use-package flymake-golangci
  :vc (:url "https://github.com/Crandel/flymake-golangci"
       :rev :newest)
  :after go-ts-mode
  :hook
  (eglot-managed-mode . (lambda()
                          (when (derived-mode-p '(go-mode go-ts-mode))
                            (flymake-golangci-load-backend)))) ;; using flymake-golangci with eglot
)
;; (require 'flycheck-rcp)

;; (use-package flymake-flycheck
;;   :ensure t
;;   :after flycheck
;;   :preface
;;   (defun vd/enable-flymake-flycheck ()
;;     (setq-local flymake-diagnostic-functions
;;                 (seq-uniq (append flymake-diagnostic-functions
;;                                   (flymake-flycheck-all-chained-diagnostic-functions)))))
;;   :config
;;   (setq-default
;;      flycheck-disabled-checkers
;;      (append (default-value 'flycheck-disabled-checkers)
;;              '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package sh-shellcheck)))
;;   :hook
;;   (flymake-mode . vd/enable-flymake-flycheck)
;; )

(provide 'flymake-rcp)

;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; flymake-rcp.el ends here
