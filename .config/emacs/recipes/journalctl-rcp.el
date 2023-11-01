;;; journalctl-rcp.el --- Major mode to view journalctl's output in Emacs

;;; Code:
(eval-when-compile (require 'use-package))
(use-package journalctl-mode
  :ensure t
  :bind
  :bind (
  ("C-c t" . journalctl-boot)
  :map evil-normal-state-map
  ("gtt" . journalctl-boot)
  ("gtu" . journalctl-unit)
  ("gts" . journalctl-user-unit)
  ("gta" . journalctl-add-opt)
  ("gtr" . journalctl-remove-opt)
  ("gte" . journalctl-edit-opts)
  )
)
(provide 'journalctl-rcp)

;;; Commentary:
;;
;;; journalctl-rcp.el ends here
