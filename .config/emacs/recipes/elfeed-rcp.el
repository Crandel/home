;;; elfeed-rcp.el --- RSS feeds in Emacs
;;; Code:
(eval-when-compile (require 'use-package))
(use-package elfeed
  :ensure t
  :defer t
  :preface
  (defun vd/elfeed-startup (switch)
    (elfeed))
  :init
  (add-to-list 'command-switch-alist '("elfeed" . vd/elfeed-startup))
  :custom
  (url-queue-timeout 30)
  (elfeed-search-filter "@4-week-ago +unread ")
  :bind
  (:map elfeed-search-mode-map
        ("a" . elfeed-update))
        ("C-q" . save-buffers-kill-terminal)
  :config
  (elfeed-update)
  (run-with-timer 0 (* 60 15) 'elfeed-update)
)

(use-package elfeed-summary
  :ensure t
  :after elfeed
  :commands (elfeed-summary)
  :custom
  (elfeed-summary-filter-by-title t)
)

(use-package elfeed-goodies
  :ensure t
  :after elfeed
  :config
  (elfeed-goodies/setup)
  (evil-collection-define-key '(normal visual) 'elfeed-search-mode-map
    "J" 'elfeed-goodies/split-show-next
    "K" 'elfeed-goodies/split-show-prev)
  (evil-collection-define-key '(normal visual) 'elfeed-show-mode-map
    "J" 'elfeed-goodies/split-show-next
    "K" 'elfeed-goodies/split-show-prev)
  :custom
  (elfeed-goodies/entry-pane-size 0.5)
)

(use-package elfeed-tube
  :ensure t
  :after elfeed
  :config
  (elfeed-tube-setup)
)

(use-package elfeed-tube-mpv
  :ensure t
  :after elfeed-tube
)

(provide 'elfeed-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; elfeed-rcp.el ends here
