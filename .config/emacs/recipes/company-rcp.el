;;; company-rcp.el --- Company autocomplete

;;; Code:
(use-package company
    :ensure t
    :defer 1
    :config
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
    (company-dabbrev-code-everywhere   t)
    (company-dabbrev-code-ignore-case  nil)
    (company-selection-wrap-around     t)
    (company-tooltip-align-annotations t)
    (selection-coding-system           'utf-8)
    (company-format-margin-function    #'company-vscode-dark-icons-margin)
    (company-auto-commit-chars         '(32 40 41 119 46 34 36 47 124 33))
    (company-backends '(
                         (
                          company-capf
                          :with company-yasnippet
                          company-files
                          company-dabbrev-code
                          )
                         company-semantic
                         company-etags
                         company-keywords
                         company-dabbrev))
    :bind
    (:map company-active-map
                ("C-h" . nil))
)

(use-package company-flx
  :ensure t
  :hook (company-mode . company-flx-mode)
)

(provide 'company-rcp)

;;; Commentary:
;;
;;; company-rcp.el ends here
