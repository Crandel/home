(defun my_python_hooks()
    (interactive)
    (setq tab-width 4)
    (setq python-indent 4)
    (setq-default py-shell-name "ipython")
    (setq-default py-which-bufname "IPython")
    (if (string-match-p "rita" (or (buffer-file-name) ""))
        (setq indent-tabs-mode t)
      (setq indent-tabs-mode nil)
    )
    ;(add-to-list 'company-backends 'company-jedi)
)
(add-hook 'python-mode-hook 'my_python_hooks)


(provide 'my_python)

