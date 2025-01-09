;;; marginalia-rcp.el --- This package provides marginalia-mode which adds marginalia to the minibuffer completions.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package marginalia
  :ensure t
  :demand t
  :custom
  (marginalia-align       'left)
  (marginalia-field-width 20)
  (marginalia-annotator-registry
   '((command marginalia-annotate-command marginalia-annotate-binding builtin)
     (embark-keybinding marginalia-annotate-embark-keybinding builtin)
     (customize-group marginalia-annotate-customize-group builtin)
     (variable marginalia-annotate-variable builtin)
     (function marginalia-annotate-function marginalia-annotate-minor-mode builtin)
     (face marginalia-annotate-face builtin)
     (color marginalia-annotate-color builtin)
     (unicode-name marginalia-annotate-char builtin)
     (minor-mode marginalia-annotate-minor-mode builtin)
     (symbol marginalia-annotate-symbol builtin)
     (environment-variable marginalia-annotate-environment-variable
                           builtin)
     (input-method marginalia-annotate-input-method builtin)
     (coding-system marginalia-annotate-coding-system builtin)
     (charset marginalia-annotate-charset builtin)
     (package marginalia-annotate-package builtin)
     (imenu marginalia-annotate-imenu builtin)
     (bookmark marginalia-annotate-bookmark builtin)
     (file marginalia-annotate-file builtin)
     (project-file marginalia-annotate-project-file builtin)
     (buffer marginalia-annotate-buffer)
     (library marginalia-annotate-library builtin)
     (theme marginalia-annotate-theme builtin)
     (tab marginalia-annotate-tab builtin)
     (multi-category marginalia-annotate-multi-category builtin)))

  :config
  (marginalia-mode)
)

(provide 'marginalia-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; marginalia-rcp.el ends here
