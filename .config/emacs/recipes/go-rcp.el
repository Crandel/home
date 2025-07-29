;;; go-rcp.el --- Golang support

;;; Code:
(eval-when-compile (require 'use-package))
(use-package go-ts-mode
  :mode "\\.go\\'"
  :preface
  (defun vd/go-ts-var-is-global-p (node)
    "Predicate which tests whether a `var_spec' node belongs to the global scope"
    ;; The parent node for `var_spec' which is `var_declaration' belongs to the
    ;; global scope, if the `var_declaration' node belongs to a parent node of
    ;; type `source_file'.
    (let* ((var-declaration (treesit-parent-until
                             node
                             (lambda (item)
                               (string-equal (treesit-node-type item) "var_declaration"))
                             t))
           (var-declaration-parent (treesit-node-parent var-declaration)))
      (string-equal (treesit-node-type var-declaration-parent) "source_file")))
  (defun vd/go-ts-const-is-global-p (node)
    "Predicate which tests whether a `const_spec' node belongs to the global scope"
    ;; The parent node for `const_spec' which is `const_declaration' belongs to the
    ;; global scope, if the `const_declaration' node belongs to a parent node of
    ;; type `source_file'.
    (let* ((const-declaration (treesit-parent-until
                               node
                               (lambda (item)
                                 (string-equal (treesit-node-type item) "const_declaration"))
                               t))
           (const-declaration-parent (treesit-node-parent const-declaration)))
      (string-equal (treesit-node-type const-declaration-parent) "source_file")))
  (defun vd/go-ts-const-spec-node-name (node)
    "Returns the name of a `const_spec' node"
    (let ((const-name (treesit-node-child-by-field-name node "name")))
      (treesit-node-text const-name)))
  (defun vd/go-ts-var-spec-node-name (node)
    "Returns the name of a `var_spec' node"
    (let ((var-name (treesit-node-child-by-field-name node "name")))
      (treesit-node-text var-name)))
  (defun vd/go-mode-setup ()
    (setq-default eglot-workspace-configuration
                  '((:gopls .
                            ((staticcheck     . t)
                             (usePlaceholders . t)
                             (local           . "github.com/talon-one/")
                             (symbolScope     . "workspace")
                             (analyses        . (
                                                 (nilness . t)
                                                 (shadow . t)
                                                 (unusedwrite . t)
                                                 (escape . t)
                                                 ))
                             (hints . ((parameterNames . t)))
                             )
                            ))
                  )
    (setq-local imenu-max-item-length 200)
    (setq-local imenu-index-create-function #'treesit-simple-imenu)
    (setq-local treesit-simple-imenu-settings
                '(
                  ("Constants" "\\`const_spec\\'" vd/go-ts-const-is-global-p vd/go-ts-const-spec-node-name)
                  ("Variables" "\\`var_spec\\'" vd/go-ts-var-is-global-p vd/go-ts-var-spec-node-name)
                  ("Interfaces" "\\`type_declaration\\'" go-ts-mode--interface-node-p nil)
                  ("Types" "\\`type_declaration\\'" go-ts-mode--other-type-node-p nil)
                  ("Functions" "\\`function_declaration\\'" nil nil)
                  ("Structs" "\\`type_declaration\\'" go-ts-mode--struct-node-p nil)
                  ("Aliases" "\\`type_declaration\\'" go-ts-mode--alias-node-p nil)
                  ("Methods" "\\`method_declaration\\'" nil nil))
                )
    (setq-local consult-imenu-config
                '((go-ts-mode :toplevel "Constants"
                              :types (
                                      (?c "Constants"  font-lock-constant-face)
                                      (?v "Variables"  font-lock-variable-name-face)
                                      (?i "Interfaces" font-lock-type-face)
                                      (?a "Aliases"    font-lock-type-face)
                                      (?t "Types"      font-lock-type-face)
                                      (?s "Structs"    font-lock-type-face)
                                      (?f "Functions"  font-lock-function-name-face)
                                      (?m "Methods"    font-lock-function-name-face)
                                      )
                              ))
                )
    )
  :bind
  (:map go-ts-mode-map
    ("C-c i a" . treesit-beginning-of-defun)
    ("C-c i e" . treesit-end-of-defun)
    ("C-c i t" . go-ts-mode-test-function-at-point)
    ("C-c i f" . go-ts-mode-test-this-file)
    ("C-c i p" . go-ts-mode-test-this-package)
    ("RET"     . reindent-then-newline-and-indent)
    ("M-RET"   . newline)
   )
  :custom
  (go-ts-mode-indent-offset 4)
  :config
  (add-to-list 'exec-path "~/.local/bin")
  :hook
  (go-ts-mode . vd/go-mode-setup)
)

(use-package go-mod-ts-mode
  :mode "\\go.mod\\'"
)

(use-package crandel-go-tag
  :vc (:url "https://github.com/Crandel/emacs-go-tag"
       :rev :newest)
  :after go-ts-mode
  :bind
  (:map go-ts-mode-map
    ("C-c i r" . go-tag-remove)
    ("C-c i i" . go-tag-add)
    )
)

;; (use-package godoctor
;;   :ensure t
;; )

(provide 'go-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; go-rcp.el ends here
