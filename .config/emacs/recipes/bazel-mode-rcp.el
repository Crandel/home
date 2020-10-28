;;; bazel-mode-rcp.el --- Bazel support for emacs

;;; Code:
(use-package bazel-mode
  :ensure t
  :mode ("\\.star\\'" . bazel-starlark-mode)
)

(provide 'bazel-mode-rcp)
;;; Commentary:
;;
;;; bazel-mode-rcp.el ends here
