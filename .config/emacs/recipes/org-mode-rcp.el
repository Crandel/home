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
  (defun vd/org-insert-headings ()
    "Insert a new heading with same level as current, after current subtree."
    (interactive)
    (org-insert-heading-after-current)
    (meow-insert)
    )
  (defun vd/org-insert-item ()
    "Insert item."
    (interactive)
    (org-insert-item)
    (meow-insert)
    )
  :custom
  (org-M-RET-may-split-line         nil)
  (org-confirm-babel-evaluate       nil)
  (org-edit-src-content-indentation 0)
  (org-ellipsis                     "•")
  (org-goto-auto-isearch            nil)
  (org-hide-emphasis-markers        nil) ;; show all markers
  (org-src-fontify-natively         t)
  (org-src-preserve-indentation     t "do not put two spaces on the left")
  (org-src-strip-leading-and-trailing-blank-lines t)
  (org-src-tab-acts-natively        t)
  (org-src-tab-acts-natively        t)
  (org-src-window-setup             'current-window "edit in current window")
  (org-support-shift-select         'always)
  (org-yank-folded-subtrees         nil)
  (word-wrap                        nil)
  :init
  (with-eval-after-load 'meow
    (setq meow-org-motion-keymap (make-keymap))
    (meow-define-state org-motion
      "Org-mode structural motion"
      :lighter "[N]"
      :keymap meow-org-motion-keymap)

    (meow-define-keys 'org-motion
      '("<escape>" . meow-normal-mode)
      '("i" . meow-insert-mode)
      '("u" .  meow-undo)
      ;; Moving between headlines
      '("h" .  org-previous-visible-heading)
      '("l" .  org-next-visible-heading)
      ;; Moving between headings at the same level
      '("k" .  org-backward-heading-same-level)
      '("j" .  org-forward-heading-same-level)
      ;; Moving subtrees themselves
      '("K" .  org-move-subtree-up)
      '("J" .  org-move-subtree-down)
      ;; de/promotion
      '("L" .  org-demote-subtree)
      '("H" .  org-promote-subtree)
      '("D" .  org-do-demote)
      '("P" .  org-do-promote)
      ;; Completion-style search of headings
      '("v" .  consult-org-heading)
      ;; Setting subtree metadata
      '("p" .  org-set-property)
      '("t" .  org-todo)
      '("d" .  org-deadline)
      '("s" .  org-schedule)
      '("e" .  org-set-effort)
      ;; Block navigation
      '("b" .  org-previous-block)
      '("f" .  org-next-block)
      '("g" .  org-goto)
      ;; Narrowing/widening
      '("N" .  org-narrow-to-subtree)
      '("W" .  widen))
    (meow-define-keys 'normal
      '("N" . meow-org-motion-mode))
    )
  :config
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src elisp"))
  (add-to-list 'org-structure-template-alist '("p" . "src sql"))
  (add-to-list 'org-structure-template-alist '("m" . "src markdown"))
  (add-to-list 'org-src-lang-modes           '("conf-unix" . conf-unix))
  (remove-hook 'org-cycle-hook
              #'org-optimize-window-after-visibility-change)
  (org-babel-do-load-languages 'org-babel-load-languages vd/org-babel-load-languages)
  :bind
  (:map org-mode-map
  ("TAB" . org-cycle)
  ("C-c o o" . vd/org-toggle-emphasis)
  ("C-c o t" . org-insert-structure-template)
  ("C-c o a" . org-babel-remove-result)
  ("C-c o i" . vd/org-insert-item)
  ("C-c o h" . vd/org-insert-headings)
    )
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
                (add-to-list 'org-structure-template-alist '("g" . "src go"))
                ))
)

(provide 'org-mode-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; org-mode-rcp.el ends here
