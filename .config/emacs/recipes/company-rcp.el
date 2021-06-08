;;; company-rcp.el --- Company autocomplete

;;; Code:
(use-package company
    :ensure t
    :defer 1
    :init
    (defun my-sort-uppercase (candidates)
      (let (case-fold-search
            (re "\\`[[:upper:]]*\\'"))
        (sort candidates
              (lambda (s1 s2)
                (and (string-match-p re s2)
                     (not (string-match-p re s1)))))))
    (defun my-change-company-backends (backend)
      (unless (member backend company-backends)
        (message "company add backend %s to %S" backend company-backends)
        (push backend company-backends))
      )
    :config
    (push 'my-sort-uppercase company-transformers)
    (global-company-mode t)
    (bind-chord "ew" 'company-abort 'company-active-map)
    :custom
    (company-idle-delay                0)
    (delete-selection-mode             t)
    (company-minimum-prefix-length     2)
    (company-dabbrev-downcase          nil)
    (company-dabbrev-other-buffers     t)
    (company-echo-delay                0)
    (company-show-numbers              t)
    (company-selection-wrap-around     t)
    (company-tooltip-align-annotations t)
    (selection-coding-system           'utf-8)
    (company-format-margin-function    #'company-vscode-dark-icons-margin)
    (company-auto-commit-chars         '(32 40 41 119 46 34 36 47 124 33))
    (company-backends '(
                        company-files
                        company-dabbrev
                        (
                         company-capf
                         :with company-yasnippet
                         )
                        ))
    :bind(
      :map company-active-map
      ("C-h" . nil)
      ([tab] . 'company-indent-or-complete-common)
      :map company-mode-map
      ("C-h" . nil)
      ([tab] . 'company-indent-or-complete-common)
    )
)

(use-package company-flx
  :ensure t
  :hook (company-mode . company-flx-mode)
)

(provide 'company-rcp)

;;; Commentary:
;;
;;; company-rcp.el ends here
