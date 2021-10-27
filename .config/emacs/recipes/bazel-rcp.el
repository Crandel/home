;;; bazel-mode-rcp.el --- Bazel support for emacs

;;; Code:
(eval-when-compile (require 'use-package))
(use-package bazel
  :ensure t
  :mode ("\\.star\\'" . bazel-starlark-mode)
)

(provide 'bazel-rcp)
;;; Commentary:
;;
;;; bazel-rcp.el ends here
