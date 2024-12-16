;;; corfu-rcp.el --- Corfu enhances the default completion in region function with a completion overlay.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package corfu
  :ensure t
  :preface
  (defun vd/corfu-disable-quit-with-orderless-advice (func)
    (let ((corfu-quit-at-boundary
           (not (seq-contains-p (car corfu--input)
                                corfu-separator))))
      (funcall func)))
  :custom
  (text-mode-ispell-word-completion nil)
  (corfu-auto             t)
  (corfu-auto-delay       0.1)
  (corfu-auto-prefix      2)
  (corfu-count            10)
  (corfu-cycle            t)
  (corfu-preselect        'first)
  (corfu-preview-current  t)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match    t)
  :bind(
  :map corfu-map
       ("<esc>" . corfu-quit)
       ("C-f"   . corfu-quick-complete)
  )
  :init
  (global-corfu-mode)
  (corfu-indexed-mode)
  (corfu-history-mode)
  (corfu-echo-mode)
  (advice-add 'corfu--auto-post-command :around #'vd/corfu-disable-quit-with-orderless-advice)
)

(use-package cape
  :ensure t
  :functions cape-capf-super cape-file
  :preface
  (defun vd/setup-lsp-completion ()
    (message "inside setup lsp completion")
    (setq-local completion-at-point-functions (list (cape-capf-super #'tempel-complete
                                                                     (cape-capf-buster #'eglot-completion-at-point #'string-prefix-p)
                                                                     #'cape-file)))
    )
  (defun vd/setup-elisp-completion ()
    (setq-local completion-at-point-functions (list (cape-capf-super #'tempel-complete
                                                                     #'elisp-completion-at-point
                                                                     #'cape-file)))
    )
  :init
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  :hook
  (emacs-lisp-mode     . vd/setup-elisp-completion)
  (eglot-managed-mode  . vd/setup-lsp-completion)
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
