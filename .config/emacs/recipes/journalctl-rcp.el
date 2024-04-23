;;; journalctl-rcp.el --- Major mode to view journalctl's output in Emacs

;;; Code:
(eval-when-compile (require 'use-package))
(use-package journalctl-mode
  :ensure t
  :commands journalctl
  :custom
  (journalctl-chunk-size 500)
  (journalctl-default-options '("--lines=500" "--reverse"))
  :bind (
  ("C-c t j" . journalctl)
  )
)

(provide 'journalctl-rcp)

;;; Commentary:
;;
;;; journalctl-rcp.el ends here
