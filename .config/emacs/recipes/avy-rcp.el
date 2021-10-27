;;; avy-rcp.el --- Jump to arbitrary positions in visible text and select text quickly.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package avy
  :ensure t
  :defer t
  :bind
  ("C-_" . 'avy-goto-char)
  ("C-/" . 'avy-goto-char)
  ("C-c /" . 'avy-goto-line)
  :custom
  (avy-all-windows 'all-frames)
)

(provide 'avy-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; avy-rcp.el ends here
