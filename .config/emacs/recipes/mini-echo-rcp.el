;;; mini-echo-rcp.el --- Show buffer status in echo area , get rid of mode-line!

;;; Code:
(eval-when-compile (require 'use-package))
(use-package mini-echo
  :ensure t
  :after hide-mode-line
  :preface
  (defun mini-echo-persistent-detect ()
    "Return a plist of persistent rule if matched.
    Otherwise, return nil."
    (with-current-buffer (current-buffer)
      ;; NOTE return the first match, so the former has higher priority
      (pcase major-mode
        ('elfeed-search-mode '(:both ("elfeed")))
        ('helpful-mode       '(:both ("major-mode" "helpful")))
        (_ nil))))
  :custom-face
  (mini-echo-major-mode        ((t (:foreground "green"))))
  (mini-echo-meow              ((t (:foreground "yellow"))))
  (mini-echo-minibuffer-window ((t nil)))
  :custom
  (mini-echo-persistent-function 'mini-echo-persistent-detect)
  (mini-echo-persistent-rule '(:long (
                                        "time"
                                        "buffer-position"
                                        "major-mode"
                                        "flycheck"
                                        "eglot"
                                        "vcs"
                                        "shrink-path"
                                        "macro"
                                        "meow"
                                        )
                                :short (
                                         "time"
                                         "major-mode"
                                         "eglot"
                                         "buffer-name-short"
                                         "meow"
                                         ))
                               )
  (mini-echo-buffer-status-style 'both)
  (mini-echo-window-divider-args '(t 1 1))
  (mini-echo-update-interval     0.3)
  :config
  (mini-echo-mode)
  )

(use-package hide-mode-line
  :ensure t
  :init
  (setq hide-mode-line-excluded-modes nil)
)
(provide 'mini-echo-rcp)
;;; Commentary:
;;
;;; mini-echo-rcp.el ends here
