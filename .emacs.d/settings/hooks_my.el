;; Python mode
(defun my-merge-imenu ()
  (interactive)
  (let ((mode-imenu (imenu-default-create-index-function))
        (custom-imenu (imenu--generic-function imenu-generic-expression)))
    (append mode-imenu custom-imenu)))

(defun my-python-hooks()
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
    (eval-after-load "company"
        '(progn
            (unless (member 'company-jedi (car company-backends))
                (setq comp-back (car company-backends))
                (push 'company-jedi comp-back)
                (setq company-backends (list comp-back)))
            )))

(add-hook 'python-mode-hook 'my-python-hooks)

;; Web mode
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; Po mode
(autoload 'po-mode "po-mode"
             "Major mode for translators to edit PO files" t)
(setq auto-mode-alist (cons '("\\.po\\'\\|\\.po\\." . po-mode)
                        auto-mode-alist))
(autoload 'po-find-file-coding-system "po-compat")
(modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\."
                            'po-find-file-coding-system)

(defun my-lisp-hooks()
    (progn
     (eval-after-load "company"
              '(progn
                   (unless (member 'company-elisp (car company-backends))
                            (setq comp-back (car company-backends))
                            (push 'company-elisp comp-back)
                            (setq company-backends (list comp-back)))
                   ))))
;; Lisp mode
(add-hook 'lisp-interaction-mode-hook 'my-lisp-hooks)

(defadvice yes-or-no-p (around hack-exit (prompt))
   (if (string= prompt "Active processes exist; kill them and exit anyway? ")
       t
      ad-do-it))

;; company sorting
(defun my-sort-uppercase (candidates)
  (let (case-fold-search
        (re "\\`[[:upper:]]*\\'"))
    (sort candidates
          (lambda (s1 s2)
            (and (string-match-p re s2)
                 (not (string-match-p re s1)))))))
(push 'my-sort-uppercase company-transformers)

(provide 'hooks_my)
