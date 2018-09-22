(defun my-change-company-backends (backend)
  (unless (member backend (car company-backends))
                                      (setq comp-back (car company-backends))
                                      (push backend comp-back)
                                      (setq company-backends (list comp-back)))
)

;; Python mode
(defun my-merge-imenu ()
  (interactive)
  (let ((mode-imenu (imenu-default-create-index-function))
        (custom-imenu (imenu--generic-function imenu-generic-expression)))
    (append mode-imenu custom-imenu)))

(defun insert_ipdb ()
  (interactive)
  (progn
    (move-end-of-line nil)
    (newline-and-indent)
    (insert "import ipdb; ipdb.set_trace()")))

;; Before save hook
(defun my-before-save-hook ()
  (when (eq major-mode 'python-mode)
    (progn
       (delete-trailing-whitespace))))

(add-hook 'before-save-hook #'my-before-save-hook)

(add-hook 'python-mode-hook '(lambda()
                               (interactive)
                               (setenv "TERM" "ansi-term")

                               (setq python-shell-completion-native nil
                                     indent-tabs-mode               t
                                     tab-width                      4
                                     python-indent                  4
                                     python-shell-interpreter       "ipython"
                                     python-shell-interpreter-args  "--profile=emacs"
                                     )

                               (defun python-startup-function (start end &optional send-main msg)
                                 (unless (python-shell-get-process)
                                   (run-python)))

                               (add-function :before (symbol-function 'python-shell-send-region)  #'python-startup-function)

                               (if (string-match-p "rita" (or (buffer-file-name) ""))
                                   (setq indent-tabs-mode t)
                                 (setq indent-tabs-mode nil)
                                 )

                               (add-to-list
                                'imenu-generic-expression
                                '("Sections" "^#### \\[ \\(.*\\) \\]$" 1))

                               (setq imenu-create-index-function 'my-merge-imenu)

                               ;; pythom mode keybindings
                               (define-key python-mode-map (kbd "M-.") 'jedi:goto-definition)
                               (define-key python-mode-map (kbd "M-,") 'jedi:goto-definition-pop-marker)
                               (define-key python-mode-map (kbd "C-c C-b") 'insert_ipdb)
                               (define-key python-mode-map (kbd "RET") 'newline-and-indent)
                               (define-key python-mode-map (kbd "M-RET") 'newline)
                               (define-key python-mode-map (kbd "M-/") 'jedi:show-doc)
                               (define-key python-mode-map (kbd "M-?") 'helm-jedi-related-names)
                               ;; end python mode keybindings

                               (eval-after-load "company"
                                 '(progn
                                    (my-change-company-backends 'company-jedi)))))
;; End Python mode

;; Po mode
(autoload 'po-mode "po-mode"
  "Major mode for translators to edit PO files" t)
(setq auto-mode-alist (cons '("\\.po\\'\\|\\.po\\." . po-mode)
                            auto-mode-alist))
(autoload 'po-find-file-coding-system "po-compat")
(modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\."
                            'po-find-file-coding-system)
;; End Po mode

;; Json mode
(add-hook 'json-mode-hook (lambda ()
                            (define-key json-mode-map (kbd "RET") 'newline-and-indent)
                            (define-key json-mode-map (kbd "M-RET") 'newline)
                            ))

;; Lisp mode
(add-hook 'lisp-interaction-mode-hook '(lambda()
                                         (progn
                                           (define-key lisp-interaction-mode-map (kbd "RET") 'newline-and-indent)
                                           (define-key lisp-interaction-mode-map (kbd "M-RET") 'newline)
                                           (eval-after-load "company"
                                             '(progn
                                                (my-change-company-backends 'company-elisp)
                                           ))
)))
;; End Lisp mode

;; c-mode hooks
(add-hook 'c-mode-hook '(lambda()
                              (progn
                                (local-unset-key (kbd "C-d"))
                                (define-key c-mode-map (kbd "RET") 'newline-and-indent)
                                (define-key c-mode-map (kbd "M-RET") 'newline)
                                )))
;; End c-mode

;; Buffer-menu-mode-hook
(add-hook 'buffer-menu-mode-hook '(lambda()
                                    (let ((font-lock-unfontify-region-function
                                           (lambda (start end)
                                             (remove-text-properties start end '(font-lock-face nil)))))
                                      (font-lock-unfontify-buffer)
                                      (set (make-local-variable 'font-lock-defaults)
                                           '(buffer-menu-buffer-font-lock-keywords t))
                                      (font-lock-fontify-buffer))))
;; End buffer-menu-mode-hook

;; Compilation hook
(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not
        (with-current-buffer buffer
          (search-forward "warning" nil t))))
      (delete-other-windows)
    (switch-to-buffer buffer)
    (delete-other-windows)
    ))
(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)
;; End compilation hook
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defadvice yes-or-no-p (around hack-exit (prompt))
  (if (string= prompt "Active processes exist; kill them and exit anyway? ")
      t
    ad-do-it))

;; company sorting
(defun my-sort-uppercase (candidates)
  (let (case-fold-search
        (re "\\`[[:upper:]].*[[:upper:]].*"))
    (sort candidates
          (lambda (s1 s2)
            (and (string-match-p re s2)
                 (not (string-match-p re s1)))))))
(push 'my-sort-uppercase company-transformers)
;; end company sorting

(setq buffer-menu-buffer-font-lock-keywords
      '(("^....[*]Man .*Man.*"    . font-lock-variable-name-face) ; Man page
        (".*.py"                  . font-lock-comment-face)         ; Python
        (".*.el"                  . font-lock-doc-face)             ; Emacs Lisp
        (".*Dired.*"              . font-lock-comment-face)         ; Dired
        ("^....[*]shell.*"        . font-lock-preprocessor-face)  ; shell buff
        (".*[*]scratch[*].*"      . font-lock-function-name-face) ; scratch buffer
        ("^....[*].*"             . font-lock-string-face)        ; "*" named buffers
        ("^..[*].*"               . font-lock-constant-face)      ; Modified
        ("^.[%].*"                . font-lock-keyword-face)         ; Read only
        ))

(add-to-list 'auto-mode-alist '("\\.hql\\'" . sql-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.fish\\'" . fish-mode))


(provide 'hooks_my)
