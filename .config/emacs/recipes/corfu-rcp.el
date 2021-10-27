;;; corfu-rcp.el --- Corfu enhances the default completion in region function with a completion overlay.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package corfu
  :ensure t
  :init
  (corfu-global-mode)
  :custom
  (corfu-cycle            t)
  (corfu-auto             t)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match    t)
  (corfu-auto-prefix      1)
  (corfu-auto-delay       0.2)
  (completion-at-point-functions '(elisp-completion-at-point comint--complete-file-name-data)
                                 comint-completion-addsuffix nil)

)

(provide 'corfu-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; corfu-rcp.el ends here
