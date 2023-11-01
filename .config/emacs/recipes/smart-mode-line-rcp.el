;;; smart-mode-line-rcp.el --- A color coded smart mode-line.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package smart-mode-line
  :ensure t
  :demand t
  :custom-face
  (mode-line-buffer-id-inactive ((t (:inherit mode-line-buffer-id :foreground "dark goldenrod"))))
  (sml/read-only ((t (:inherit sml/not-modified :foreground "deep sky blue"))))
  :config
  (sml/setup)
  (setq-default mode-line-format (list
                                  '("%e"
                                    evil-mode-line-tag
                                    mode-line-modified
                                    sml/pre-id-separator
                                    mode-line-buffer-identification
                                    sml/pos-id-separator
                                    )
                                  ;;mode-line-front-space
                                  '("%e"
                                    (vc-mode vc-mode)
                                    )
                                   ;; line and column
                                    mode-line-misc-info
                                    " "
                                    )
                )
  :custom
  (sml/debug                     nil)
  (sml/no-confirm-load-theme     t)
  (sml/pre-id-separator          "{")
  (sml/pos-id-separator          "}")
  (sml/pre-minor-modes-separator " ")
  (sml/pos-minor-modes-separator "%")
  (sml/pre-modes-separator       "#")
  (sml/pos-modes-separator       "&")
  (sml/shorten-directory         t)
  (sml/shorten-modes             t)
  (sml/show-frame-identification nil)
  (sml/use-projectile-p          'before-prefixes)
  (sml/vc-mode-show-backend      nil)
  (sml/theme                     'respectful)
  )



(provide 'smart-mode-line-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; smart-mode-line-rcp.el ends here
