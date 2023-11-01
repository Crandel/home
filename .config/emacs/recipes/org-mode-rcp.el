;;; org-mode-rcp.el --- A GNU Emacs major mode for convenient plain text markup — and much more.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package org
  :ensure t
  :pin elpa
  :preface
  (setq vd/org-babel-load-languages '(
                                      (emacs-lisp . t)
                                      (python     . t)
                                      (shell      . t)
                                      (sql        . t)
                                      ))
  (defun vd/org-toggle-emphasis ()
    "Toggle hiding/showing of org emphasize markers."
    (interactive)
    (if org-hide-emphasis-markers
        (set-variable 'org-hide-emphasis-markers nil)
      (set-variable 'org-hide-emphasis-markers t))
    (org-mode-restart))
  :custom
  (org-confirm-babel-evaluate       nil)
  (org-edit-src-content-indentation 0)
  (org-ellipsis                     "▾")
  (org-hide-emphasis-markers        nil) ;; show all markers
  (org-src-fontify-natively         t)
  (org-support-shift-select         'always)
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
  :hook
  (evil-local-mode . (lambda ()
    (evil-global-set-key 'normal "grt" 'vd/org-toggle-emphasis)
    (evil-global-set-key 'normal "gri" 'org-insert-structure-template)
    (evil-global-set-key 'normal "gra" 'org-babel-remove-result)
  ))
)

(use-package org-tempo
  :after org
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
  (org-local-mode . (lambda()
                (add-to-list 'org-structure-template-alist '("r" . "src restclient"))
                ))
)

(use-package ob-go
  :ensure t
  :mode ("\\.goorg\\'" . org-mode)
  :init
  (add-to-list 'vd/org-babel-load-languages '(go . t))
  :hook
  (org-local-mode . (lambda()
                (add-to-list 'org-structure-template-alist '("go" . "src go"))
                ))
)

(provide 'org-mode-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; org-mode-rcp.el ends here
