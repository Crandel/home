;;; evil-rcp.el --- Evil mode

;;; Code:
(use-package evil
  :ensure t
  :bind (:map evil-normal-state-map
         ("C-p" . helm-multi-files))
  :config
  ;; Enable evil-mode in all buffers.
  (evil-mode 1))

(provide 'evil-rcp)

;;; Commentary:
;; 
;;; evil-rcp.el ends here
