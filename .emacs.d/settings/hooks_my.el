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

(add-hook 'python-mode-hook '(lambda()
                               (interactive)
                               (setenv "TERM" "ansi-term")
                               (setq python-shell-completion-native nil
                                     indent-tabs-mode                 t
                                     tab-width                      4
                                     python-indent                  4
                                     python-shell-interpreter         "ipython"
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
                               (define-key python-mode-map (kbd "C-c C-b") 'insert_ipdb)
                               (define-key python-mode-map (kbd "RET") 'newline-and-indent)
                               (define-key python-mode-map (kbd "M-RET") 'newline)
                               (define-key python-mode-map (kbd "M-,") 'jedi:goto-definition-pop-marker)
                               (define-key python-mode-map (kbd "M-/") 'jedi:show-doc)
                               (define-key python-mode-map (kbd "M-?") 'helm-jedi-related-names)
                               ;; end python mode keybindings

                               (eval-after-load "company"
                                 '(progn
                                    (unless (member 'company-jedi (car company-backends))
                                      (setq comp-back (car company-backends))
                                      (push 'company-jedi comp-back)
                                      (setq company-backends (list comp-back)))))
                               ))
;; End Python mode

;; Web mode
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.gotmpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.gtpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.edi\\'" . edi-mode))

;; End Web mode

;; Po mode
(autoload 'po-mode "po-mode"
  "Major mode for translators to edit PO files" t)
(setq auto-mode-alist (cons '("\\.po\\'\\|\\.po\\." . po-mode)
                            auto-mode-alist))
(autoload 'po-find-file-coding-system "po-compat")
(modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\."
                            'po-find-file-coding-system)
;; End Po mode

;; Js2 mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
;; End Js2 mode

;; Lisp mode
(add-hook 'lisp-interaction-mode-hook '(lambda()
                                         (progn
                                           (define-key lisp-interaction-mode-map (kbd "RET") 'newline-and-indent)
                                           (define-key lisp-interaction-mode-map (kbd "M-RET") 'newline)
                                           (eval-after-load "company"
                                             '(progn
                                                (unless (member 'company-elisp (car company-backends))
                                                  (setq comp-back (car company-backends))
                                                  (push 'company-elisp comp-back)
                                                  (setq company-backends (list comp-back)))
                                                )))))
;; End Lisp mode

;; Go mode
; go get -u github.com/alecthomas/gometalinter
; gometalinter --install
; go get -u github.com/rogpeppe/godef
; go get -u github.com/nsf/gocode
; go get -u github.com/kardianos/govendor
(add-hook 'go-mode-hook '(lambda()
                           (progn
                             (setq gofmt-command    "goimports"
                                   indent-tabs-mode t
                                   tab-width        2)
                             (add-hook 'before-save-hook #'gofmt-before-save)
                             ;; Go mode keybindings
                             (define-key go-mode-map (kbd "M-.") #'godef-jump)
                             (define-key go-mode-map (kbd "RET") 'newline-and-indent)
                             (define-key go-mode-map (kbd "M-RET") 'newline)
                             ;; End keybindings
                             (eval-after-load "company"
                               '(progn
                                  (unless (member 'company-go (car company-backends))
                                    (setq comp-back (car company-backends))
                                    (push 'company-go comp-back)
                                    (setq company-backends (list comp-back)))
                                  )))))
;; End Go mode

;; Smartparents hooks
(add-hook 'smartparens-mode-hook '(lambda()
                                    (sp-with-modes '(markdown-mode gfm-mode rst-mode)
                                      (sp-local-pair "*" "*")
                                      (sp-local-pair "**" "**")
                                      (sp-local-pair "_" "_" ))
                                    (sp-with-modes '(web-mode)
                                      (sp-local-pair "%" "%")
                                      (sp-local-pair "<" ">"))))
;; End smartparens hooks

;; Restclient hooks
(add-hook 'restclient-mode-hook '(lambda()
                                  (progn
                                    (eval-after-load "company"
                                      '(progn
                                          (unless (member 'company-restclient (car company-backends))
                                            (setq comp-back (car company-backends))
                                            (push 'company-restclient comp-back)
                                            (setq company-backends (list comp-back)))
                                          )))))

;; End restclient hooks

;; Web-mode hook
(add-hook 'web-mode-hook '(lambda()
                            (progn
                              (define-key web-mode-map (kbd "RET") 'newline-and-indent)
                              (define-key web-mode-map (kbd "M-RET") 'newline)
                              (eval-after-load "company"
                                '(progn
                                   (unless (member 'company-css (car company-backends))
                                     (setq comp-back (car company-backends))
                                     (push 'company-css comp-back)
                                     (push 'company-nxml comp-back)
                                     (setq company-backends (list comp-back)))
                                  )))))

;; End Web-mode hook

;; Scala mode hook
(add-hook 'scala-mode-hook '(lambda()
                              (progn
                                (ensime-mode)
                                (local-unset-key (kbd "M-."))
                                (local-unset-key (kbd "M-,"))
                                (local-unset-key [tab])
                                (define-key scala-mode-map (kbd "RET") 'newline-and-indent)
                                (define-key scala-mode-map (kbd "M-RET") 'newline)
                                (define-key scala-mode-map (kbd "TAB") nil)
                                (define-key scala-mode-map (kbd "TAB") nil)
                                (define-key scala-mode-map (kbd "M-.") 'scala-syntax:beginning-of-definition)
                                (define-key scala-mode-map (kbd "M-,") 'scala-syntax:end-of-definition)
                                (define-key ensime-mode-map [tab] 'tab-indent-or-complete)
                                (define-key scala-mode-map [tab] 'tab-indent-or-complete)
                                )))
(add-hook 'ensime-mode-hook '(lambda()
                              (progn
                                (local-unset-key (kbd "M-n"))
                                (local-unset-key (kbd "M-m"))
                                (local-unset-key (kbd "C-c v"))
                                (local-unset-key (kbd "C-c C-m"))
                                (local-unset-key [tab])
                                (define-key scala-mode-map [tab] nil)
                                (define-key scala-mode-map (kbd "TAB") nil)
                                (define-key ensime-mode-map (kbd "M-n") 'mc/mark-next-like-this)
                                (define-key ensime-mode-map (kbd "M-m") 'mc/mark-previous-like-this)
                                (define-key ensime-mode-map (kbd "C-c e") 'ensime)
                                (define-key ensime-mode-map (kbd "C-c v") 'ensime-refactor-diff-extract-local)
                                (define-key ensime-mode-map (kbd "C-c C-m") 'ensime-refactor-diff-extract-method)
                                (define-key ensime-mode-map [tab] 'tab-indent-or-complete)
                                (define-key scala-mode-map (kbd "TAB") 'tab-indent-or-complete)
                                )))
(add-to-list 'auto-mode-alist '("\\.sc\\'" . scala-mode))
;; End scala mode hook

;; java-mode hooks
(add-hook 'java-mode-hook '(lambda()
                              (progn
                                (local-unset-key (kbd "C-d"))
                                (local-unset-key (kbd "C-c e"))
                                (define-key java-mode-map (kbd "C-c e") 'meghanada-mode)
                                (define-key java-mode-map (kbd "RET") 'newline-and-indent)
                                (define-key java-mode-map (kbd "M-RET") 'newline)
                                ;(meghanada-mode t)
                                )))
;; End java-mode
;; c-mode hooks
(add-hook 'c-mode-hook '(lambda()
                              (progn
                                (local-unset-key (kbd "C-d"))
                                (define-key c-mode-map (kbd "RET") 'newline-and-indent)
                                (define-key c-mode-map (kbd "M-RET") 'newline)
                                )))
;; End c-mode
;; Sh
(add-to-list 'auto-mode-alist '("\\.bashrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.fish\\'" . fish-mode))

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


(provide 'hooks_my)
