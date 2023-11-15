;;; awesome-tray-rcp.el --- Hide mode-line, display necessary information at right of minibuffer.
;;; Code:
(eval-when-compile (require 'use-package))
(use-package awesome-tray
  :vc (:fetcher github :repo Crandel/awesome-tray)
  :functions awesome-tray-mode
  :config
  ;; Fix lsp header-line
  (set-face-attribute 'header-line nil
                      :foreground (face-attribute 'mode-line :foreground)
                      :background (face-attribute 'mode-line :background)
                      ;; :height of mode-line is also unspecified, so we set it directly.
                      :height 150
                      :box (face-attribute 'mode-line :box))
  (awesome-tray-mode t)
  :custom
  (awesome-tray-active-modules '(
                                 "evil"
                                 "buffer-read-only"
                                 "buffer-name"
                                 "git"
                                 "belong"
                                 "file-path"
                                 "mode-name"
                                 "last-command"
                                 "flymake"
                                 "location-or-page"
                                 "date"))
  (awesome-tray-date-format            "%-H:%-M")
  (awesome-tray-evil-show-cursor-count t)
  (awesome-tray-evil-show-mode         t)
  (awesome-tray-git-show-status        t)
  (awesome-tray-hide-mode-line         t)
  (awesome-tray-second-line            t)
  (awesome-tray-update-interval        0.6)
)
(provide 'awesome-tray-rcp)

;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; awesome-tray-rcp.el ends here
