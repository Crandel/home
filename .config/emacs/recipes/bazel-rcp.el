;;; bazel-mode-rcp.el --- Bazel support for emacs

;;; Code:
(use-package bazel
  :ensure t
  :mode ("\\.star\\'" . bazel-starlark-mode)
)

(provide 'bazel-rcp)
;;; Commentary:
;;
;;; bazel-rcp.el ends here
