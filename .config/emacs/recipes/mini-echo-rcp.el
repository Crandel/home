;;; mini-echo-rcp.el --- Show buffer status in echo area , get rid of mode-line!

;;; Code:
(eval-when-compile (require 'use-package))
(use-package mini-echo
  :ensure t
  :after hide-mode-line
  :custom-face
  (mini-echo-major-mode        ((t (:foreground "green"))))
  (mini-echo-meow              ((t (:foreground "yellow"))))
  (mini-echo-minibuffer-window ((t nil)))
  :custom
  (mini-echo-default-segments '(:long (
                                        "time"
                                        "buffer-position"
                                        "major-mode"
                                        "flycheck"
                                        "vcs"
                                        "buffer-name"
                                        "macro"
                                        "meow"
                                        )
                                :short (
                                         "time"
                                         "major-mode"
                                         "buffer-name-short"
                                         "meow"
                                         ))
                               )
  (mini-echo-buffer-status-style 'both)
  (mini-echo-window-divider-args '(t 1 1))
  (mini-echo-update-interval 0.3)
  :init
  (setq mini-echo-rules
      '((elfeed-search-mode :long (("elfeed"   . 2)))
        (go-ts-mode         :long (("lsp-mode" . 4)))
        (rust-ts-mode       :long (("lsp-mode" . 4)))
        (c-ts-mode          :long (("lsp-mode" . 4)))
        (python-ts-mode     :long (("lsp-mode" . 4)))
        ))
  :config
  (mini-echo-mode 1)
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
