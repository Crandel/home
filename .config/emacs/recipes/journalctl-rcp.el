;;; journalctl-rcp.el --- Major mode to view journalctl's output in Emacs

;;; Code:
(eval-when-compile (require 'use-package))
(use-package journalctl-mode
  :ensure t
  :commands journalctl
  :bind (
  ("C-c t j" . journalctl)
  )
)

(provide 'journalctl-rcp)

;;; Commentary:
;;
;;; journalctl-rcp.el ends here
