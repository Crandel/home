;; Python mode
(defun my-merge-imenu ()
  (interactive)
  (let ((mode-imenu (imenu-default-create-index-function))
        (custom-imenu (imenu--generic-function imenu-generic-expression)))
    (append mode-imenu custom-imenu)))

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
    (add-to-list
        'imenu-generic-expression
        '("Sections" "^#### \\[ \\(.*\\) \\]$" 1))
    (setq imenu-create-index-function 'my-merge-imenu)
    (setq company-backends '((company-yasnippet company-files company-anaconda company-semantic company-css company-etags) company-gtags company-keywords company-dabbrev-code company-nxml)))

(add-hook 'python-mode-hook 'my_python_hooks)

;; Web mode
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(provide 'my_hooks)

