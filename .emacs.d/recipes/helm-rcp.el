;;; helm-rcp.el --- Helm is an Emacs incremental and narrowing framework

;;; Commentary:
;; 

;;; Code:

(use-package helm
  :ensure t
  :preface (require 'helm-config)
  :custom-face
  (helm-buffer-modified ((t (:inherit font-lock-comment-face :foreground "coral"))))
  :config
  (helm-autoresize-mode 1)
  :custom
  (helm-split-window-in-side-p           t) ; open helm buffer inside current window, not occupy whole other window
  (helm-split-window-default-side        'below)
  (helm-completion-in-region-fuzzy-match t)
  (helm-candidate-number-limit           100)
  (helm-move-to-line-cycle-in-source     t) ; move to end/beginning of source when reaching top or bottom of source.
  (helm-case-fold-search                 nil)
  (helm-autoresize-max-height            50)
  (helm-autoresize-min-height            10)
  (helm-recentf-fuzzy-match              t)
  :bind
  ("M-x" . helm-M-x)
  ("C-x x" . execute-extended-command)
  ("M-y" . helm-show-kill-ring)
  ("C-c m" . helm-all-mark-rings)
  (:map helm-map
        ("C-v" . yank)
        ("<right>" . helm-next-source)
        ("<left>" . helm-previous-source))
)

(use-package helm-mode
  :hook
  (helm-mode . (lambda ()
                      (setq completion-styles
                            (cond ((assq 'helm-flex completion-styles-alist)
                                   '(helm-flex)) ;; emacs-26.
                                  ((assq 'flex completion-styles-alist)
                                   '(flex)))))) ;; emacs-27+.
  :diminish (helm-mode " ")
  :after helm
  :custom
  (helm-mode-fuzzy-match t)
  :config
  (helm-mode 1)
  (add-to-list 'helm-completing-read-handlers-alist '(find-file . ido))
  (add-to-list 'helm-completing-read-handlers-alist '(find-file-read-only . ido))
)

(use-package helm-adaptive
  :after helm-mode
  :config
  (helm-adaptive-mode 1)
  :custom
  (helm-adaptive-history-file nil)
)

(use-package helm-buffers
  :after helm-mode
  :custom
  (helm-buffers-favorite-modes (append helm-buffers-favorite-modes '(picture-mode artist-mode)))
  (helm-buffers-fuzzy-matching       t)
  (helm-buffer-skip-remote-checking  t)
  (helm-buffer-max-length            22)
  (helm-buffers-end-truncated-string "…")
  (helm-buffers-maybe-switch-to-tab  t)
  (helm-mini-default-sources '(helm-source-buffers-list
                              helm-source-buffer-not-found))
  :bind
  ("C-x C-b" . helm-buffers-list)
  (:map helm-buffer-map
        ("C-v" . 'yank))
  :chords
  ("bb" . helm-buffers-list)
)

(use-package helm-imenu :bind ([f10] . 'helm-semantic-or-imenu))

(use-package helm-info
  :bind ("C-h r" . helm-info-emacs))

(use-package helm-files
  :custom
  (helm-ff-auto-update-initial-value        t)
  (helm-ff-allow-non-existing-file-at-point t)
  (helm-ff-search-library-in-sexp           t) ; search for library in `require' and `declare-function' sexp.
  (helm-ff-file-name-history-use-recentf    t)
  (helm-ff-lynx-style-map                   t)
  (helm-trash-remote-files                  t)
  (helm-dwim-target                         'next-window)
  (helm-ff-cache-mode-post-delay            0.3)
  (helm-ff-refresh-cache-delay              0.3)
  (helm-ff-skip-boring-files                nil)
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
  :bind (
  ("C-p" . helm-multi-files)
  ("C-x C-f" . helm-find-files)
  (:map helm-read-file-map
        ("C-/" . helm-ff-run-find-sh-command)
        ("C-d" . helm-ff-persistent-delete)
        ("C-i" . nil)
        ("RET" . helm-ff-RET)
        ))
)

(use-package helm-lib
  :custom
  (helm-scroll-amount                    8) ; scroll 8 lines other window using M-<next>/M-<prior>
  (helm-advice-push-mark                 nil)
)

(use-package helm-utils
  :after helm-mode
  :config
  ;; Popup buffer-name or filename in grep/moccur/imenu-all etc...
  (helm-popup-tip-mode 1)
  :custom
  (helm-highlight-matches-around-point-max-lines   30)
  (helm-window-show-buffers-function #'helm-window-mosaic-fn)
)

(use-package helm-sys
  :after helm-mode
  :commands (helm-top)
  :config (helm-top-poll-mode 1)
)


;; 3-d party packages

(use-package helm-company
  :ensure t
  :after (company)
  :bind (
  (:map company-mode-map
        ("C-:" . helm-company))
  (:map company-active-map
        ("C-:" . helm-company)))
)

(use-package helm-descbinds
  :ensure t
  :config
  (helm-descbinds-mode 1)
)

(use-package helm-lsp
  :ensure t
  :after helm-mode
  :bind ("C-j" . helm-lsp-code-actions))

(use-package helm-projectile
  :ensure t
  :bind ("M-p" . 'helm-projectile-ag))

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

(provide 'helm-rcp)

;;; helm-rcp.el ends here
