;;; corfu-rcp.el --- Corfu enhances the default completion in region function with a completion overlay.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package corfu
  :ensure t
  :preface
  (defun corfu-lsp-setup ()
        (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :custom
  (corfu-auto             t)
  (corfu-auto-delay       0.1)
  (corfu-auto-prefix      2)
  (corfu-count            20)
  (corfu-cycle            t)
  (corfu-preselect-first  t)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match    t)
  :bind(
  :map corfu-map
       ("M-c" . corfu-quick-complete)
  )
  :init
  (global-corfu-mode)
  (corfu-indexed-mode)
  :hook
  (lsp-completion-mode . corfu-lsp-setup)
)

(use-package cape
  :ensure t
  :functions cape-super-capf
  :preface
  (defun vd/setup-elisp-completion ()
    (setq-local completion-at-point-functions
                `(,(cape-super-capf
                    #'elisp-completion-at-point
                    #'cape-dabbrev)
                  cape-file)
                ))
  (defun vd/setup-lsp-completion ()
    (setq-local completion-at-point-functions (list (cape-super-capf #'lsp-completion-at-point
                                                                     #'cape-keyword
                                                                     #'cape-abbrev)))
    )
  :init
  (add-hook 'completion-at-point-functions (cape-super-capf #'cape-dabbrev #'cape-abbrev #'cape-keyword) nil nil)
  :hook
  (emacs-lisp-mode . vd/setup-elisp-completion)
  (lsp-completion-mode . vd/setup-lsp-completion)
)

(use-package kind-icon
  :ensure t
  :after corfu
  :functions kind-icon-margin-formatter
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  :custom
  (kind-icon-default-face 'corfu-default)
)

(use-package corfu-terminal
  :ensure t
  :after corfu
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1))
)

(provide 'corfu-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; corfu-rcp.el ends here
