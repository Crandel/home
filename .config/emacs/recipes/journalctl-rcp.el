;;; journalctl-rcp.el --- Major mode to view journalctl's output in Emacs

;;; Code:
(eval-when-compile (require 'use-package))
;; (use-package journalctl-mode
;;   :vc (:fetcher github :repo WJCFerguson/journalctl)
;;   :bind
;;   :bind (
;;   ("C-c t" . journalctl)
;;   :map evil-normal-state-map
;;   ("gtt" . journalctl)
;;   ("gtf" . jcm-full-message)
;;   )
;; )

(use-package journalctl-mode
  :ensure t
  :bind (
  ("C-c t" . journalctl)
  :map evil-normal-state-map
  ("gtt" . journalctl)
  )
  :hook
  (journalctl-mode . turn-off-evil-mode)
)

(provide 'journalctl-rcp)

;;; Commentary:
;;
;;; journalctl-rcp.el ends here
