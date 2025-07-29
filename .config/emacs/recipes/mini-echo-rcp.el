;;; mini-echo-rcp.el --- Show buffer status in echo area , get rid of mode-line!

;;; Code:
(eval-when-compile (require 'use-package))
(use-package mini-echo
  :ensure t
  :after hide-mode-line
  :preface
  (defun vd/mini-echo-minibuffer-width-lessp ()
    "Return non-nil if current minibuffer window width less than 120."
    (< (mini-echo-minibuffer-width) 100))
  (defun vd/mini-echo-persistent-detect ()
    "Return a plist of persistent rule if matched.
    Otherwise, return nil."
    (with-current-buffer (current-buffer)
      ;; NOTE return the first match, so the former has higher priority
      (pcase major-mode
        ('elfeed-search-mode '(:both ("time" "elfeed")))
        ('helpful-mode       '(:both ("time" "major-mode" "helpful")))
        (_ nil))))
  :custom-face
  (mini-echo-major-mode        ((t (:foreground "green"))))
  (mini-echo-meow              ((t (:foreground "yellow"))))
  (mini-echo-minibuffer-window ((t nil)))
  :custom
  (mini-echo-short-style-predicate 'vd/mini-echo-minibuffer-width-lessp)
  (mini-echo-persistent-function   'vd/mini-echo-persistent-detect)
  (mini-echo-persistent-rule       '(
    :short
    (
     "time"
     "major-mode"
     "flymake"
     "eglot"
     "buffer-name"
     "meow"
     )
    :long
    (
     "time"
     "buffer-position"
     "major-mode"
     "flymake"
     "eglot"
     "vcs"
     "shrink-path"
     "macro"
     "meow"
     )
    )
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
