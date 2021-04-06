;;; flycheck-rcp.el --- On-the-fly syntax checking

;;; Code:
(use-package flycheck
  :ensure t
  :defer t
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save idle-change))
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
;;
;;; flycheck-rcp.el ends here
