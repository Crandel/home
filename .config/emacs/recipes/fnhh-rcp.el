;;; fnhh-rcp.el --- Garbage collector for emacs

;;; Commentary:
;; 

;;; Code:

(use-package fnhh
  :quelpa
  (fnhh :repo "a13/fnhh" :fetcher github)
  :config
  (fnhh-mode 1))

(provide 'fnhh-rcp)

;;; fnhh-rcp.el ends here
