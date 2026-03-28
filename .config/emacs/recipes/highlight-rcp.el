;;; highlight-rcp.el --- Minor modes for highlighting things.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package highlight-indentation
  :ensure t
  :defer t
  :config
  (custom-set-faces
   '(highlight-indentation-face ((t (:inherit 'unspecified :foreground "IndianRed" :background nil)))))
  :hook
  (prog-mode    . highlight-indentation-mode)
  (yaml-mode    . highlight-indentation-mode)
  (toml-ts-mode . highlight-indentation-mode)
)

(use-package highlight-defined
  :ensure t
  :defer t
  :hook
  ((emacs-lisp-mode lisp-interaction-mode) . highlight-defined-mode)
)

(provide 'highlight-rcp)
;;; Commentary:
;;
;;; highlight-rcp.el ends here
