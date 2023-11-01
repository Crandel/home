;;; flymake-rcp.el --- Continuous syntax checking for Emacs.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package flymake
  :bind ("C-c C-n" . flymake-show-buffer-diagnostics)
  :config
  (flymake-mode t)
  :hook
  (prog-mode . flymake-mode)
  (text-mode . flymake-mode)
  (web-mode  . flymake-mode)
)

(use-package flymake-golangci
  :ensure t
  :after go
  :hook
  (go-mode . flymake-golangci-load)
)
(require 'flycheck-rcp)

(use-package flymake-flycheck
  :ensure t
  :after flycheck
  :preface
  (defun vd/enable-flymake-flycheck ()
    (setq-local flymake-diagnostic-functions
                (seq-uniq (append flymake-diagnostic-functions
                                  (flymake-flycheck-all-chained-diagnostic-functions)))))
  :config
  (setq-default
     flycheck-disabled-checkers
     (append (default-value 'flycheck-disabled-checkers)
             '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package sh-shellcheck)))
  :hook
  (flymake-mode . vd/enable-flymake-flycheck)
)

(provide 'flymake-rcp)

;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;;; flymake-rcp.el ends here
