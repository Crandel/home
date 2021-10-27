;;; flycheck-rcp.el --- On-the-fly syntax checking

;;; Code:
(eval-when-compile (require 'use-package))
(use-package flycheck
  :ensure t
  :defer t
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save idle-change))
  (flycheck-emacs-lisp-load-path       'inherit)
  (flycheck-highlighting-mode          'lines)
  (flycheck-indication-mode            'left-fringe)
  (flycheck-scalastylerc               "~/scalastyle_config.xml")
  (flycheck-checker-error-threshold    2000)
  :hook
  (python-mode           . flycheck-mode)
  (js-mode               . flycheck-mode)
  (web-mode              . flycheck-mode)
  (lisp-interaction-mode . flycheck-mode)
  (emacs-lisp-mode       . flycheck-mode)
  (fish-mode             . flycheck-mode)
  (markdown-mode         . flycheck-mode)
  (go-mode               . flycheck-mode)
  (scala-mode            . flycheck-mode)
  (java-mode             . flycheck-mode)
  (c-mode                . flycheck-mode)
  (c++-mode              . flycheck-mode)
  (rust-mode             . flycheck-mode)
  :bind ("C-c n" . flycheck-next-error)
)

(provide 'flycheck-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; flycheck-rcp.el ends here
