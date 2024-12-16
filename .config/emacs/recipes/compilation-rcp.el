;;; compilation-rcp.el --- A minimalist package that enhances compilation-mode in the following ways.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package fancy-compilation
  :ensure t
  :commands (fancy-compilation-mode)
  :custom
  (fancy-compilation-scroll-output 'first-error)
  :init
  (with-eval-after-load 'compile
    (fancy-compilation-mode))
)

(provide 'compilation-rcp)
;;; Commentary:
;;
;;; compilation-rcp.el ends here
