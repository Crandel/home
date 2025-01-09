;;; dape-rcp.el --- Debug Adapter Protocol for Emacs.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package dape
  :ensure t
  :init
  (setq dape-key-prefix (kbd "C-c r"))
  :custom
  (dape-buffer-window-arrangement 'gud)
  (dape-debug                     t)
  (dape-inlay-hints               t)
  (dape-cwd-fn                    #'projectile-project-root)
  :config
  ;; Projectile users
  (add-to-list 'dape-configs
               `(dlv-debug
                 modes (go-mode go-ts-mode)
                 ensure dape-ensure-command
                 command "dlv"
                 command-args ("dap" "--listen" "127.0.0.1::autoport")
                 command-cwd dape-command-cwd
                 command-insert-stderr t
                 port :autoport
                 :request "launch"
                 :type "debug"       ;; needed to set the adapterID correctly as a string type
                 :cwd dape-cwd-fn
                 :program dape-cwd-fn))
  (add-to-list 'dape-configs
               `(dlv-test
                 modes (go-mode go-ts-mode)
                 ensure dape-ensure-command
                 fn (dape-config-autoport dape-config-tramp)
                 command "dlv"
                 command-args ("dap" "--listen" "127.0.0.1::autoport")
                 command-cwd (lambda()(if (string-suffix-p "_test.go" (buffer-name))
                                          default-directory (dape-cwd)))
                 port :autoport
                 :type "debug"
                 :request "launch"
                 :mode (lambda() (if (string-suffix-p "_test.go" (buffer-name)) "test" "debug"))
                 :program "."
                 :cwd "."
                 :args (lambda()
                         (require 'which-func)
                         (if (string-suffix-p "_test.go" (buffer-name))
                             (when-let* ((test-name (which-function))
                                         (test-regexp (concat "^" test-name "$")))
                               (if test-name `["-test.run" ,test-regexp]
                                 (error "No test selected")))
                           []))))
  :hook
  ;; Save breakpoints on quit
  ((kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
   (after-init . dape-breakpoint-load))
)
(provide 'dape-rcp)
;;; Commentary:
;;
;;; dape-rcp.el ends here
