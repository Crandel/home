;;; company-rcp.el --- Company autocomplete

;;; Code:
(use-package company
    :ensure t
    :defer 0.1
    :init
    ;; (defun my-sort-uppercase (candidates)
    ;;   (let (case-fold-search
    ;;         (re "\\`[[:upper:]]*\\'"))
    ;;     (sort candidates
    ;;           (lambda (s1 s2)
    ;;             (and (string-match-p re s2)
    ;;                  (not (string-match-p re s1)))))))
    (defun my-change-company-backends (backend)
      (unless (member backend (car company-backends))
        (setq comp-back (car company-backends))
        (push backend comp-back)
        (setq company-backends (list comp-back)))
      )
    :config
    ;; (push 'my-sort-uppercase company-transformers)
    (global-company-mode t)
    (bind-chord "ew" 'company-abort 'company-active-map)
    :custom
    (company-auto-commit-chars         '(32 40 41 119 46 34 36 47 124 33))
    (company-dabbrev-downcase          nil)
    (company-dabbrev-ignore-case       nil)
    (company-dabbrev-other-buffers     t)
    (company-echo-delay                0.1)
    (company-format-margin-function    #'company-vscode-dark-icons-margin)
    (company-idle-delay                0.1)
    (company-minimum-prefix-length     1)
    (company-selection-wrap-around     t)
    (company-show-numbers              t)
    (company-tooltip-align-annotations t)
    (delete-selection-mode             t)
    (selection-coding-system           'utf-8)
    (company-backends '(
                        (
                         company-capf
                         :with
                         company-yasnippet
                         :with
                         company-dabbrev-code
                         :separate
                         company-files
                         :separate
                         company-dabbrev
                         :separate company-keywords
                        )
                        ))
    :bind(
      :map company-active-map
      ("<tab>" . company-complete-common-or-cycle)
      ("<backtab>" . (lambda() (interactive) (company-complete-common-or-cycle -1)))
      ("C-j" . company-select-next-or-abort)
      ("C-k" . company-select-previous-or-abort)
      ("C-l" . company-other-backend)
      ("C-h" . nil)
      :map company-mode-map
      ("C-h" . nil)
    )
)

(use-package company-prescient
  :ensure t
  :custom
  (company-prescient-sort-length-enable t)
)

(provide 'company-rcp)

;;; Commentary:
;;
;;; company-rcp.el ends here
