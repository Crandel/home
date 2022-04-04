;;; web-mode-rcp.el --- major mode for editing web templates

;;; Code:
(eval-when-compile (require 'use-package))
(use-package web-mode
  :ensure t
  :defer t
  :mode
  ("\\.js\\'"     . web-mode)
  ("\\.html\\'"   . web-mode)
  ("\\.css\\'"    . web-mode)
  ("\\.djhtml\\'" . web-mode)
  ("\\.gotmpl\\'" . web-mode)
  ("\\.tmpl\\'"   . web-mode)
  ("\\.gtpl\\'"   . web-mode)
  ("\\.vue\\'"    . web-mode)
  ("\\german_lang/index.html\\'" . mhtml-mode)
  :custom-face
  (web-mode-block-face ((t nil)))
  :custom
  (web-mode-engines-alist '(
                            ("django" . "\\.html\\'")
                            ("razor"  . "\\.scala.html\\'")
                            ("go"     . "\\.\\(gtpl\\|tmpl\\)'")
                            ("jsx"    . "\\.js\\'")))
  (web-mode-enable-current-element-highlight   t)
  (web-mode-enable-block-face                  t)
  (web-mode-markup-indent-offset               2)   ;; HTML
  (web-mode-css-indent-offset                  2)   ;; CSS
  (web-mode-code-indent-offset                 2)   ;; JavaScript
  (web-mode-enable-part-face                   t)
  (web-mode-enable-comment-keywords            t)
  (web-mode-enable-css-colorization            t)
  (web-mode-enable-auto-pairing                nil)
  (web-mode-enable-auto-indentation            nil)
  (web-mode-enable-current-column-highlight    t)
  :bind
  (:map web-mode-map
        ("RET" . newline-and-indent)
        ("M-RET" . newline)
        )
)

(provide 'web-mode-rcp)
;;; Commentary:
;;
;;; web-mode-rcp.el ends here
