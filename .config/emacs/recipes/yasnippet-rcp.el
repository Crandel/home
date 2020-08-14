;;; yasnippet-rcp.el --- Yet another snippet extension for Emacs

;;; Commentary:
;; 

;;; Code:

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  ;; (defun check-expansion ()
  ;;   (save-excursion
  ;;     (if (looking-at "\\_>") t
  ;;       (backward-char 1)
  ;;       (if (looking-at "\\.") t
  ;;         (backward-char 1)
  ;;         (if (looking-at "->") t nil)))))
  ;; (defun do-yas-expand ()
  ;;   (let ((yas/fallback-behavior 'return-nil))
  ;;     (yas/expand)))
  ;; (defun tab-indent-or-complete ()
  ;;   (interactive)
  ;;   (message (minibufferp))
  ;;   (if (minibufferp)
  ;;       (minibuffer-complete)
  ;;     (if (or (not yas/minor-mode)
  ;;             (null (do-yas-expand)))
  ;;         (if (check-expansion)
  ;;             (company-complete-common)
  ;;           (indent-for-tab-command)))))
  ;; :bind
  ;; ([tab] . tab-indent-or-complete)
  :hook
  (c++-mode              . yas-minor-mode)
  (c-mode                . yas-minor-mode)
  (emacs-lisp-mode       . yas-minor-mode)
  (fish-mode             . yas-minor-mode)
  (go-mode               . yas-minor-mode)
  (java-mode             . yas-minor-mode)
  (js-mode               . yas-minor-mode)
  (lisp-interaction-mode . yas-minor-mode)
  (markdown-mode         . yas-minor-mode)
  (python-mode           . yas-minor-mode)
  (rust-mode             . yas-minor-mode)
  (scala-mode            . yas-minor-mode)
  (web-mode              . yas-minor-mode)
  )

(use-package yafolding :ensure t)

(use-package yasnippet-snippets :ensure t)

(provide 'yasnippet-rcp)

;;; yasnippet-rcp.el ends here
