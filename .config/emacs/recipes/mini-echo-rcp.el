;;; mini-echo-rcp.el --- Show buffer status in echo area , get rid of mode-line!

;;; Code:
(eval-when-compile (require 'use-package))
(use-package mini-echo
  :ensure t
  :vc (:fetcher github :repo Crandel/mini-echo.el)
  :custom-face
  (mini-echo-major-mode        ((t (:foreground "green"))))
  (mini-echo-minibuffer-window ((t nil)))
  :custom
  (mini-echo-default-segments '(:long (
                                        "time"
                                        "buffer-position"
                                        "major-mode"
                                        "lsp-mode"
                                        "flymake"
                                        "vcs"
                                        "buffer-name"
                                        "evil"
                                        )
                                :short (
                                         "time"
                                         "major-mode"
                                         "buffer-name-short"
                                         "evil"
                                         ))
                               )
  (mini-echo-buffer-status-style 'both)
  (mini-echo-window-divider-args '(t 1 1))
  (mini-echo-update-interval 0.3)
  :config
  (mini-echo-mode 1)
)

(provide 'mini-echo-rcp)
;;; Commentary:
;;
;;; mini-echo-rcp.el ends here
