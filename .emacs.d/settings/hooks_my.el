;; Python mode
(defun my-merge-imenu ()
  (interactive)
  (let ((mode-imenu (imenu-default-create-index-function))
        (custom-imenu (imenu--generic-function imenu-generic-expression)))
    (append mode-imenu custom-imenu)))

(add-hook 'python-mode-hook '(lambda()
                                 (interactive)
                                 (setenv "TERM" "ansi-term")
                                 (setq python-shell-completion-native nil
                                       tab-width                      4
                                       python-indent                  4
                                       warning-minimum-level          :emergency
                                       python-shell-interpreter       "ipython"
                                       python-shell-interpreter-args  "-i")
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
                                 (define-key python-mode-map (kbd "M-<return>") 'newline)
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
;; End Js2 mode

;; Lisp mode
(add-hook 'lisp-interaction-mode-hook '(lambda()
                                           (progn
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
      (".*.py"                  . font-lock-comment-face)       ; Python
      (".*.el"                  . font-lock-doc-face)           ; Emacs Lisp
      (".*Dired.*"              . font-lock-comment-face)       ; Dired
      ("^....[*]shell.*"        . font-lock-preprocessor-face)  ; shell buff
      (".*[*]scratch[*].*"      . font-lock-function-name-face) ; scratch buffer
      ("^....[*].*"             . font-lock-string-face)        ; "*" named buffers
      ("^..[*].*"               . font-lock-constant-face)      ; Modified
      ("^.[%].*"                . font-lock-keyword-face)       ; Read only
      ))


(provide 'hooks_my)
