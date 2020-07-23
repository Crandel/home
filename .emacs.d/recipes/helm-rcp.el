(use-package helm
  :ensure t
  :preface (require 'helm-config)
  :config
  (helm-autoresize-mode 1)
  ;; (add-to-list 'helm-completing-read-handlers-alist '(find-file . ido))
  ;; (add-to-list 'helm-completing-read-handlers-alist '(find-file-read-only . ido))
  :custom (
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
           (helm-boring-file-regexp-list '("\\.o$" "~$" "\\.bin$" "\\.lbin$" "\\.so$" "\\.a$" "\\.ln$" "\\.blg$" "\\.bbl$" "\\.elc$" "\\.lof$" "\\.glo$" "\\.idx$" "\\.lot$" "\\.svn\\(/\\|$\\)" "\\.hg\\(/\\|$\\)" "\\.git\\(/\\|$\\)" "\\.bzr\\(/\\|$\\)" "_darcs\\(/\\|$\\)" "_MTN\\(/\\|$\\)" "\\.fmt$" "\\.tfm$" "\\.class$" "\\.fas$" "\\.lib$" "\\.mem$" "\\.x86f$" "\\.sparcf$" "\\.dfsl$" "\\.pfsl$" "\\.d64fsl$" "\\.p64fsl$" "\\.lx64fsl$" "\\.lx32fsl$" "\\.dx64fsl$" "\\.dx32fsl$" "\\.fx64fsl$" "\\.fx32fsl$" "\\.sx64fsl$" "\\.sx32fsl$" "\\.wx64fsl$" "\\.wx32fsl$" "\\.fasl$" "\\.ufsl$" "\\.fsl$" "\\.dxl$" "\\.lo$" "\\.la$" "\\.gmo$" "\\.mo$" "\\.toc$" "\\.aux$" "\\.cp$" "\\.fn$" "\\.ky$" "\\.pg$" "\\.tp$" "\\.vr$" "\\.cps$" "\\.fns$" "\\.kys$" "\\.pgs$" "\\.tps$" "\\.vrs$" "\\.pyc$" "\\.pyo$"))
           (helm-scroll-amount                    8) ; scroll 8 lines other window using M-<next>/M-<prior>
           (helm-buffers-fuzzy-matching           t)
           (helm-case-fold-search                 nil)
           (helm-autoresize-max-height            50)
           (helm-autoresize-min-height            10)
           (helm-recentf-fuzzy-match              t))
  :bind (
         ("M-x" . 'helm-M-x)
         ("C-x x" . 'execute-extended-command)
         ("C-x C-f" . 'helm-find-files)
         ("C-p" . 'helm-multi-files)
         ([f10] . 'helm-semantic-or-imenu)
         ("M-p" . 'helm-projectile-ag)
         ("M-y" . 'helm-show-kill-ring)
         ("C-c m" . 'helm-all-mark-rings)
         ;; (:map helm-buffer-map ("C-v" . 'yank))
         (:map helm-map (
                         ("C-v" . 'yank)
                         ("<right>" . 'helm-next-source)
                         ("<left>" . 'helm-previous-source)
                         )
               )
         )
)

(provide 'helm-rcp)
