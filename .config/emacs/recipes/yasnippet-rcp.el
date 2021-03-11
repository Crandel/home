;;; yasnippet-rcp.el --- Yet another snippet extension for Emacs

;;; Code:
(use-package yasnippet
  :ensure t
  :defer t
  :init
  ;; tab indent or complete
  (defun check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil)))))

  (defun do-yas-expand ()
    (let ((yas/fallback-behavior 'return-nil))
      (yas/expand)))

  (defun tab-indent-or-complete ()
    "Check if complete needed or indent."
    (interactive)
    (message (minibufferp))
    (if (minibufferp)
        (minibuffer-complete)
      (if (or (not yas/minor-mode)
              (null (do-yas-expand)))
          (if (check-expansion)
              (company-complete-common)
            (indent-for-tab-command)))))
  :config
  (yas-reload-all)
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
  (terraform-mode        . yas-minor-mode)
  (web-mode              . yas-minor-mode)
  (yaml-mode             . yas-minor-mode)
  :bind
   ("<tab>" . tab-indent-or-complete)
)

(use-package yafolding
  :ensure t
  :defer t
)

(use-package yasnippet-snippets
  :ensure t
  :defer t
)

(provide 'yasnippet-rcp)
;;; Commentary:
;;
;;; yasnippet-rcp.el ends here
