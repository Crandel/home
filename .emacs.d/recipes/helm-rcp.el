;;; helm-rcp.el --- Helm is an Emacs incremental and narrowing framework

;;; Commentary:
;; 

;;; Code:

(use-package helm
  :ensure t
  :preface (require 'helm-config)
;  :defines helm-completing-read-handlers-alist
  :custom-face
  (helm-buffer-modified ((t (:inherit font-lock-comment-face :foreground "coral"))))
  :config
  (helm-autoresize-mode 1)
;  (add-to-list 'helm-completing-read-handlers-alist '(find-file . ido))
;  (add-to-list 'helm-completing-read-handlers-alist '(find-file-read-only . ido))
  :custom
  (helm-advice-push-mark                 nil)
  (helm-mode-fuzzy-match                 t)
  (helm-split-window-in-side-p           t) ; open helm buffer inside current window, not occupy whole other window
  (helm-split-window-default-side        'below)
  (helm-completion-in-region-fuzzy-match t)
  (helm-candidate-number-limit           100)
  (helm-move-to-line-cycle-in-source     t) ; move to end/beginning of source when reaching top or bottom of source.
  (helm-ff-search-library-in-sexp        t) ; search for library in `require' and `declare-function' sexp.
  (helm-ff-file-name-history-use-recentf t)
  (helm-ff-lynx-style-map                t)
  (helm-ff-skip-boring-files             nil)
  (helm-boring-file-regexp-list '("\\.o$" "~$" "\\.bin$" "\\.lbin$" "\\.so$" "\\.a$" "\\.ln$"
                                  "\\.blg$" "\\.bbl$" "\\.elc$" "\\.lof$" "\\.glo$" "\\.idx$"
                                  "\\.lot$" "\\.svn\\(/\\|$\\)" "\\.hg\\(/\\|$\\)" "\\.git\\(/\\|$\\)"
                                  "\\.bzr\\(/\\|$\\)" "_darcs\\(/\\|$\\)" "_MTN\\(/\\|$\\)" "\\.fmt$"
                                  "\\.tfm$" "\\.class$" "\\.fas$" "\\.lib$" "\\.mem$" "\\.x86f$" "\\.sparcf$"
                                  "\\.dfsl$" "\\.pfsl$" "\\.d64fsl$" "\\.p64fsl$" "\\.lx64fsl$" "\\.lx32fsl$"
                                  "\\.dx64fsl$" "\\.dx32fsl$" "\\.fx64fsl$" "\\.fx32fsl$" "\\.sx64fsl$" "\\.sx32fsl$"
                                  "\\.wx64fsl$" "\\.wx32fsl$" "\\.fasl$" "\\.ufsl$" "\\.fsl$" "\\.dxl$" "\\.lo$"
                                  "\\.la$" "\\.gmo$" "\\.mo$" "\\.toc$" "\\.aux$" "\\.cp$" "\\.fn$" "\\.ky$"
                                  "\\.pg$" "\\.tp$" "\\.vr$" "\\.cps$" "\\.fns$" "\\.kys$" "\\.pgs$"
                                  "\\.tps$" "\\.vrs$" "\\.pyc$" "\\.pyo$"))
  (helm-scroll-amount                    8) ; scroll 8 lines other window using M-<next>/M-<prior>
  (helm-buffers-fuzzy-matching           t)
  (helm-case-fold-search                 nil)
  (helm-autoresize-max-height            50)
  (helm-autoresize-min-height            10)
  (helm-recentf-fuzzy-match              t)
  :bind
  ("M-x" . 'helm-M-x)
  ("C-x x" . 'execute-extended-command)
  ("C-x C-f" . 'helm-find-files)
  ("C-p" . 'helm-multi-files)
  ([f10] . 'helm-semantic-or-imenu)
  ("M-p" . 'helm-projectile-ag)
  ("M-y" . 'helm-show-kill-ring)
  ("C-c m" . 'helm-all-mark-rings)
  (:map helm-map
        ("C-v" . 'yank)
        ("<right>" . 'helm-next-source)
        ("<left>" . 'helm-previous-source))
)

(use-package helm-lsp
 :ensure t
)

(use-package helm-projectile
 :ensure t
)

(use-package helm-buffers
  :bind
  (:map helm-buffer-map
        ("C-v" . 'yank))
)

(use-package helm-swoop
  :ensure t
  :custom
  (helm-multi-swoop-edit-save               nil)
  (helm-swoop-split-with-multiple-windows   t)
  (helm-swoop-split-direction               'split-window-horizontally)
  (helm-swoop-move-to-line-cycle            t)
  (helm-swoop-use-fuzzy-match               t)
  :bind
  ("C-c s" . helm-swoop)
  ;("C-c p r" . helm-multi-swoop-projectile)
)

(use-package helm-ag
  :ensure t
  :custom
  (helm-ag-insert-at-point 'word)
  (helm-ag-base-command    "rg --color=never -i --vimgrep")
  (helm-ag-use-temp-buffer t)
  (helm-ag-fuzzy-match     t)
  :bind
  ("C-x g" . helm-do-ag-project-root)
)

(use-package helm-company
  :ensure t
  :after (company)
  :bind (
  (:map company-mode-map
        ("C-:" . helm-company))
  (:map company-active-map
        ("C-:" . helm-company)))
)

(provide 'helm-rcp)

;;; helm-rcp.el ends here
