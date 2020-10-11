;;; smart-mode-line-rcp.el --- A color coded smart mode-line.

;;; Code:
(use-package smart-mode-line
  :ensure t
  :demand t
  :custom-face
  (mode-line-buffer-id-inactive ((t (:inherit mode-line-buffer-id :foreground "dark goldenrod"))))
  (sml/read-only ((t (:inherit sml/not-modified :foreground "deep sky blue"))))
  :config
  (sml/setup)
  :custom (
           (sml/no-confirm-load-theme     t)
           (sml/pos-id-separator          "}")
           (sml/pos-minor-modes-separator "|")
           (sml/pre-id-separator          "{")
           (sml/pre-minor-modes-separator " ")
           (sml/pre-modes-separator       "/")
           (sml/shorten-directory         t)
           (sml/shorten-modes             t)
           (sml/use-projectile-p          'before-prefixes)
           (sml/vc-mode-show-backend      nil)
           (sml/theme                     'dark))
)

(provide 'smart-mode-line-rcp)
;;; Commentary:
;;
;;; smart-mode-line-rcp.el ends here
