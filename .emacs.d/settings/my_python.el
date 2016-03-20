(add-hook 'python-mode-hook
  (lambda()
    (setq tab-width 4)
    (setq python-indent 4)
    (if (string-match-p "rita" (or (buffer-file-name) ""))
        (setq indent-tabs-mode t)
      (setq indent-tabs-mode nil)
    )
  )
)

(provide 'my_python)
