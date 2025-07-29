;;; highlight-rcp.el --- Minor modes for highlighting things.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package highlight-indentation
  :ensure t
  :defer t
  :custom-face
  (highlight-indentation-face ((t (:foreground "IndianRed"))))
  :hook
  (prog-mode . highlight-indentation-mode)
)

(use-package highlight-defined
  :ensure t
  :hook
  ((emacs-lisp-mode lisp-interaction-mode) . highlight-defined-mode)
)

(provide 'highlight-rcp)
;;; Commentary:
;;
;;; highlight-rcp.el ends here
