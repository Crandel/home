;; Python mode
(defun my-merge-imenu ()
  (interactive)
  (let ((mode-imenu (imenu-default-create-index-function))
        (custom-imenu (imenu--generic-function imenu-generic-expression)))
    (append mode-imenu custom-imenu)))

(defun my-python-hooks()
    (interactive)
    (setq tab-width     4
          python-indent 4
          python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i")
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
    (define-key python-mode-map (kbd "M-/") 'jedi:show-doc)
    (define-key python-mode-map (kbd "M-?") 'helm-jedi-related-names)
    ;; end python mode keybindings

    (eval-after-load "company"
        '(progn
            (unless (member 'company-jedi (car company-backends))
                (setq comp-back (car company-backends))
                (push 'company-jedi comp-back)
                (setq company-backends (list comp-back)))
            )))

(add-hook 'python-mode-hook 'my-python-hooks)
;; End Python mode

;; Web mode
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
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

;; Lisp mode
(defun my-lisp-hooks()
    (progn
     (eval-after-load "company"
              '(progn
                   (unless (member 'company-elisp (car company-backends))
                            (setq comp-back (car company-backends))
                            (push 'company-elisp comp-back)
                            (setq company-backends (list comp-back)))
                   ))))
(add-hook 'lisp-interaction-mode-hook 'my-lisp-hooks)
;; End Lisp mode

;; Go mode
(defun my-go-hooks()
    (progn
        (setq gofmt-command "goimports")
        (add-hook 'before-save-hook #'gofmt-before-save)
        ;; Go mode keybindings
        (define-key go-mode-map (kbd "M-.") #'godef-jump)
        ;; End keybindings
        (eval-after-load "company"
            '(progn
                 (unless (member 'company-go (car company-backends))
                     (setq comp-back (car company-backends))
                     (push 'company-go comp-back)
                     (setq company-backends (list comp-back)))
     ))))

(add-hook 'go-mode-hook 'my-go-hooks)
;; End Go mode

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

(setq buffer-menu-buffer-font-lock-keywords
    '(("^....[*]Man .*Man.*"    . font-lock-variable-name-face) ; Man page
      (".*.py"                  . font-lock-comment-face)       ; Python
      (".*.el"                  . font-lock-doc-face)           ; Emacs Lisp
      (".*Dired.*"              . font-lock-comment-face)       ; Dired
      ("^....[*]shell.*"        . font-lock-preprocessor-face)  ; shell buff
      (".*[*]scratch[*].*"      . font-lock-function-name-face) ; scratch buffer
      ("^....[*].*"             . font-lock-string-face)        ; "*" named buffers
      ("^..[*].*"               . font-lock-constant-face)      ; Modified
      ("^.[%].*"                . font-lock-keyword-face)       ; Read only
      ))

(defun buffer-menu-custom-font-lock  ()
      (let ((font-lock-unfontify-region-function
             (lambda (start end)
               (remove-text-properties start end '(font-lock-face nil)))))
        (font-lock-unfontify-buffer)
        (set (make-local-variable 'font-lock-defaults)
             '(buffer-menu-buffer-font-lock-keywords t))
        (font-lock-fontify-buffer)))

(add-hook 'buffer-menu-mode-hook 'buffer-menu-custom-font-lock)


(provide 'hooks_my)
