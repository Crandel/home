;;; http-rcp.el --- Http calls via emacs.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package verb
  :ensure t
  :mode ("\\.verb\\'" . org-mode)
  :custom
  (verb-auto-kill-response-buffers             t)
  (verb-suppress-load-unsecure-prelude-warning t)
  (url-debug                                   t)
  :init
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c v") verb-command-map))
)

(provide 'http-rcp)
;;; Commentary:
;;
;;; http-rcp.el ends here
