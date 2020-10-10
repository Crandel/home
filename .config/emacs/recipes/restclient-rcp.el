;;; restclient-rcp.el --- This is a tool to manually explore and test HTTP REST

;;; Code:
(use-package restclient
  :ensure t
  :defer t
  :mode ("\\.rest\\'" . restclient-mode)
)

(use-package restclient-helm :ensure t)

(use-package company-restclient
  :ensure t
  :defer t
  :hook (restclient-mode . (lambda()
                             (eval-after-load "company"
                               '(progn
                                  (my-change-company-backends 'company-restclient)
                                  ))
                             ))
)

(provide 'restclient-rcp)
;;; Commentary:
;;
;;; restclient-rcp.el ends here
