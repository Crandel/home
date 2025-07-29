;;; combobulate-rcp.el --- Structured Editing and Navigation in Emacs with Tree-Sitter.


;;; Code:
(eval-when-compile (require 'use-package))
(use-package combobulate
  :vc (:url "https://github.com/mickeynp/combobulate"
       :rev :newest)
  :custom
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (combobulate-key-prefix "C-c i c")
  :hook
  (go-ts-mode     . combobulate-mode)
  (python-ts-mode . combobulate-mode)
 )
(provide 'combobulate-rcp)
;;; Commentary:
;;
;;; combobulate-rcp.el ends here
