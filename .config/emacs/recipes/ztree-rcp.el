;;; ztree-rcp.el --- Ztree is a directory-diff tool for Emacs

;;; Code:
(use-package ztree
  :ensure t
  :bind (("<f9>"   . ztree-dir)
         ("C-<f9>" . ztree-diff)))

(use-package ztree-dir
  :bind (:map ztreedir-mode-map
              ("f" . ztree-dir-narrow-to-dir)
              ("b" . ztree-dir-widen-to-parent)))

(use-package ztree-view
  :bind (:map ztree-mode-map
              ("n" . next-line)
              ("p" . previous-line)))

(provide 'ztree-rcp)
;;; Commentary:
;;
;;; ztree-rcp.el ends here
