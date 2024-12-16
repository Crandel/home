;;; dape-rcp.el --- Debug Adapter Protocol for Emacs.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package dape
  :ensure t
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  (setq dape-key-prefix (kbd "C-c r"))

  :hook
  ;; Save breakpoints on quit
  ((kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
   (after-init . dape-breakpoint-load))

  :config
  ;; Turn on global bindings for setting breakpoints with mouse
  ;; (dape-breakpoint-global-mode)

  ;; Info buffers to the right
  (setq dape-buffer-window-arrangement 'right)
  (setq dape-debug t)

  ;; Info buffers like gud (gdb-mi)
  ;; (setq dape-buffer-window-arrangement 'gud)
  ;; (setq dape-info-hide-mode-line nil)

  ;; Pulse source line (performance hit)
  ;; (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

  ;; Showing inlay hints
  (setq dape-inlay-hints t)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-hook 'kill-buffer)

  ;; Projectile users
  (setq dape-cwd-fn 'projectile-project-root)
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
  )
(provide 'dape-rcp)
;;; Commentary:
;;
;;; dape-rcp.el ends here
