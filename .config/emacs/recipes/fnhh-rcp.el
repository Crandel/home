;;; fnhh-rcp.el --- Garbage collector for emacs

;;; Code:
(eval-when-compile (require 'use-package))
(use-package fnhh
  :quelpa
  (fnhh :repo "a13/fnhh" :fetcher github)
  :config
  (fnhh-mode 1))

(provide 'fnhh-rcp)
;;; Commentary:
;;
;;; fnhh-rcp.el ends here
