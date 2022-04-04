;;; org-mode-rcp.el --- A GNU Emacs major mode for convenient plain text markup — and much more.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package org
  :ensure t
  :custom
  (org-ellipsis                     "▾")
  (org-hide-emphasis-markers        t)
  (org-src-fontify-natively         t)
  (org-src-preserve-indentation     t "do not put two spaces on the left")
  (org-edit-src-content-indentation 0)
  (org-src-strip-leading-and-trailing-blank-lines t)
  (org-src-tab-acts-natively        t)
  (org-src-tab-acts-natively        t)
  (org-src-window-setup             'current-window "edit in current window")
  (truncate-lines                   t)
  (word-wrap                        nil)
  :bind (
  :map evil-normal-state-map
  ("gri"  . org-insert-structure-template)
  )
  :config
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src elisp"))
  (add-to-list 'org-structure-template-alist '("p" . "src sql"))
  (add-to-list 'org-src-lang-modes           '("conf-unix" . conf-unix))
  (remove-hook 'org-cycle-hook
              #'org-optimize-window-after-visibility-change)
  (add-to-list 'org-babel-load-languages '(emacs-lisp . t))
  (add-to-list 'org-babel-load-languages '(python     . t))
  (add-to-list 'org-babel-load-languages '(shell      . t))
  (add-to-list 'org-babel-load-languages '(sql        . t))
)

(use-package org-tempo
  :after org
)

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
)

(use-package ob-restclient
  :ensure t
  :mode ("\\.rest\\.org\\'" . org-mode)
  :hook
  (org-mode . (lambda()
  (add-to-list 'org-structure-template-alist '("r" . "src restclient"))
  (setq org-confirm-babel-evaluate nil)
  (add-to-list 'org-babel-load-languages '(restclient . t))))
)

(provide 'org-mode-rcp)
;;; Commentary:
;;
;;; org-mode-rcp.el ends here
