;;; avy-rcp.el --- Jump to arbitrary positions in visible text and select text quickly.

;;; Commentary:
;;

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

;;; avy-rcp.el ends here
