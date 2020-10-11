;;; avy-rcp.el --- Jump to arbitrary positions in visible text and select text quickly.

;;; Code:
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
;;
;;; avy-rcp.el ends here
