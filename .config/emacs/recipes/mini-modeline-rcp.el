;;; mini-modeline-rcp.el ---  Display emacs mode line in minibuffer

;;; Code:
(eval-when-compile (require 'use-package))
(use-package mini-modeline
  :ensure t
  :custom
  (mini-modeline-enhance-visual t)
  (mini-modeline-truncate-p     t)
  (mini-modeline-echo-duration  1)
  (mini-modeline-r-format '("%e"
                            evil-mode-line-tag
                            mode-line-modified
                            "{"
                            mode-line-buffer-identification
                            "}"
                            (vc-mode vc-mode)
                            ""
                            ;; line and column
                            "["
                            mode-line-misc-info
                            "]"
                            ))
  :config
  (mini-modeline-mode 1)
)

(provide 'mini-modeline-rcp)
;;; Commentary:
;;
;;; mini-modeline-rcp.el ends here
