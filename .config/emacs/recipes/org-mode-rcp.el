;;; org-mode-rcp.el --- A GNU Emacs major mode for convenient plain text markup — and much more.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package org
  :ensure t
  :preface
  (setq vd/org-babel-load-languages '(
                                      (emacs-lisp . t)
                                      (python     . t)
                                      (shell      . t)
                                      (sql        . t)
                                      ))
  :custom
  (org-confirm-babel-evaluate       nil)
  (org-edit-src-content-indentation 0)
  (org-ellipsis                     "▾")
  (org-hide-emphasis-markers        t)
  (org-src-fontify-natively         t)
  (org-src-preserve-indentation     t "do not put two spaces on the left")
  (org-src-strip-leading-and-trailing-blank-lines t)
  (org-src-tab-acts-natively        t)
  (org-src-tab-acts-natively        t)
  (org-src-window-setup             'current-window "edit in current window")
  (truncate-lines                   t)
  (word-wrap                        nil)
  :config
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src elisp"))
  (add-to-list 'org-structure-template-alist '("p" . "src sql"))
  (add-to-list 'org-src-lang-modes           '("conf-unix" . conf-unix))
  (remove-hook 'org-cycle-hook
              #'org-optimize-window-after-visibility-change)
  (org-babel-do-load-languages 'org-babel-load-languages vd/org-babel-load-languages)
)

(use-package org-tempo
  :after org
)

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
)

(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode)
)

(use-package ob-restclient
  :ensure t
  :mode ("\\.restorg\\'" . org-mode)
  :init
  (add-to-list 'vd/org-babel-load-languages '(restclient . t))
  :hook
  (org-mode . (lambda()
                (add-to-list 'org-structure-template-alist '("r" . "src restclient"))
                ))
)

(use-package ob-go
  :ensure t
  :mode ("\\.goorg\\'" . org-mode)
  :init
  (add-to-list 'vd/org-babel-load-languages '(go . t))
  :hook
  (org-mode . (lambda()
                (add-to-list 'org-structure-template-alist '("go" . "src go"))
                ))
)

(provide 'org-mode-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; org-mode-rcp.el ends here
