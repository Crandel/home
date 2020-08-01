;;; go-rcp.el --- Golang support

;;; Code:
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :custom
  (gofmt-command    "goimports")
  (indent-tabs-mode t)
  (tab-width        2)
  :hook
  (before-save-hook . gofmt-before-save)
  :bind
  (:map go-mode-map
        ("M-." . godef-jump)
        ("RET" . newline-and-indent)
        ("M-RET" . newline)
        )
)

(use-package company-go
  :ensure t
  :config
  (eval-after-load "company"
    '(progn
       (my-change-company-backends 'company-go)
       ))
  (add-to-list 'exec-path (concat default-directory "bin"))
  :custom
  (company-go-insert-arguments nil)
  :after go-mode
)

(use-package go-eldoc
  :ensure t
  :after go-mode
  :hook
  (go-mode . go-eldoc-setup)
)


(provide 'go-rcp)

;;; Commentary:
;; go get -u github.com/alecthomas/gometalinter
;; gometalinter --install
;; go get -u github.com/rogpeppe/godef
;; go get -u github.com/nsf/gocode
;;; go-rcp.el ends here
