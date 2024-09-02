;;; tempel-rcp.el --- TempEl - Simple templates for Emacs

;;; Code:
(eval-when-compile (require 'use-package))
(use-package tempel
  :ensure t
  :after cape
  :commands (tempel-expand)
  :init
  (add-to-list 'completion-at-point-functions #'tempel-complete)
  :bind (("C-t" . tempel-complete) ;; Alternative tempel-expand
         ("M-t" . tempel-insert))
)

(use-package tempel-collection
  :ensure t
  :after tempel
)

(provide 'tempel-rcp)
;;; Commentary:
;;
;;; tempel-rcp.el ends here
