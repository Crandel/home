;;; tempel-rcp.el --- TempEl - Simple templates for Emacs

;;; Code:
(eval-when-compile (require 'use-package))
(use-package tempel
  :ensure t
  :after cape
  :commands (tempel-expand)
  :preface
  (defun vd/tab-indent-or-complete ()
    (interactive)
    (message (minibufferp))
    (if (minibufferp)
        (minibuffer-complete)
      (tempel-expand)
      (indent-for-tab-command)))
  :init
  (add-to-list 'completion-at-point-functions #'tempel-complete)
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)
         ("<tab>" . vd/tab-indent-or-complete))
)

(use-package tempel-collection
  :ensure t
  :after tempel
)

(provide 'tempel-rcp)
;;; Commentary:
;;
;;; tempel-rcp.el ends here
