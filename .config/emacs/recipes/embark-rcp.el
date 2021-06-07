;;; embark-rcp.el --- This package provides a sort of right-click contextual menu for Emacs

;;; Code:
(use-package embark
  :ensure t
  :demand t
  :bind
  (("M-c" . embark-act)       ;; pick some comfortable binding
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :custom
  (prefix-help-command embark-prefix-help-command)
  (embark-action-indicator
      (lambda (map _target)
        (which-key--show-keymap "Embark" map nil nil 'no-paging)
        #'which-key--hide-popup-ignore-command)
      embark-become-indicator embark-action-indicator)
)

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :ensure t
)

(provide 'embark-rcp)
;;; Commentary:
;;
;;; embark-rcp.el ends here
