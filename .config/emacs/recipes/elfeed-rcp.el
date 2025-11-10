;;; elfeed-rcp.el --- RSS feeds in Emacs
;;; Code:
(eval-when-compile (require 'use-package))
(use-package elfeed
  :ensure t
  :preface
  (defun vd/elfeed-startup (switch)
    (elfeed))
  :init
  (add-to-list 'command-switch-alist '("elfeed" . vd/elfeed-startup))
  :custom
  (url-queue-timeout         30)
  (elfeed-use-curl           t)
  (elfeed-tube-invidious-url nil)
  (shr-use-fonts             nil) ; use default font instead of proportional
  (elfeed-search-filter      "@4-week-ago +unread ")
  :bind
  (:map elfeed-search-mode-map
        ("a"   . elfeed-update)
        ("u"   . elfeed-search-untag-all-unread)
        ("m"   . elfeed-search-browse-url)
        ("C-q" . save-buffers-kill-terminal))
  :config
  (elfeed-update)
  (run-with-timer 0 (* 60 30) 'elfeed-update)
)

(use-package elfeed-protocol
  :ensure t
  :after elfeed
  :custom
  (elfeed-protocol-enabled-protocols '(fever))
  (elfeed-protocol-fever-update-unread-only nil)
  (elfeed-protocol-fever-fetch-category-as-tag t)
  (elfeed-protocol-feeds '(("fever+http://miniflux@miniflux.local"
                               :api-url "http://miniflux.local/fever/"
                               :password "password"))
  )
  :config
  (elfeed-set-timeout 36000)
  (elfeed-protocol-enable)
)


(use-package elfeed-summary
  :ensure t
  :after elfeed
  :commands (elfeed-summary)
  :custom
  (elfeed-summary-filter-by-title t)
  :config
  (elfeed-summary)
)

(use-package elfeed-goodies
  :ensure t
  :after elfeed
  :config
  (elfeed-goodies/setup)
  :custom
  (elfeed-goodies/entry-pane-size 0.75)
  (elfeed-goodies/entry-pane-position 'top)
)

(use-package elfeed-tube
  :ensure t
  :after elfeed
  :custom
  (elfeed-tube-backend 'yt-dlp)
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
