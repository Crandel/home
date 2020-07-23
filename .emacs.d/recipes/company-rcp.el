(use-package company
    :ensure t
    :demand t
    :config (global-company-mode t)
    :custom
    (company-idle-delay                0)
    (delete-selection-mode             t)
    (company-minimum-prefix-length     2)
    (company-dabbrev-downcase          nil)
    (company-dabbrev-other-buffers     t)
    (company-echo-delay                0)
    (company-show-numbers              t)
    (company-dabbrev-code-everywhere   t)
    (company-dabbrev-code-ignore-case  t)
    (company-selection-wrap-around     t)
    (company-tooltip-align-annotations t)
    (selection-coding-system           'utf-8)
    (company-auto-complete-chars       '(32 40 41 119 46 34 36 47 124 33))
    (company-backends '((company-capf
                         company-yasnippet
                         company-semantic
                         company-files
                         company-etags
                         company-keywords
                         company-dabbrev-code
                         company-dabbrev))))

(provide 'company-rcp)
