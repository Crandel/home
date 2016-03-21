(add-hook 'python-mode-hook
  (lambda()
    (setq tab-width 4)
    (setq python-indent 4)
    (if (string-match-p "rita" (or (buffer-file-name) ""))
        (setq indent-tabs-mode t)
        (message "python tab enable")
      (setq indent-tabs-mode nil)
      (message "tab disable")
    )
  )
)

(provide 'my_python)
