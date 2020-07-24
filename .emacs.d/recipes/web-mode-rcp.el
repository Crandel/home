;;; web-mode-rcp.el --- major mode for editing web templates

;;; Commentary:
;; 

;;; Code:

(use-package web-mode
  :mode (("\\.html\\'" "\\.css\\'" "\\.djhtml\\'" "\\.gotmpl\\'" "\\.gtpl\\'" "\\.vue\\'" . web-mode)
         ("\\german_lang/index.html\\'" . html-mode))
  :custom
  (web-mode-engines-alist '(
                            ("django" . "\\.html\\'")
                            ("razor" . "\\.scala.html\\'")
                            ("go" . "\\.gotmpl\\'")
                            ("jsx" . "\\.js\\'")))
  (web-mode-enable-current-element-highlight   t)
  (web-mode-enable-block-face                  t)
  (web-mode-markup-indent-offset               2)   ;; HTML
  (web-mode-css-indent-offset                  2)   ;; CSS
  (web-mode-code-indent-offset                 2)   ;; JavaScript
  (web-mode-enable-part-face                   t)
  (web-mode-enable-comment-keywords            t)
  (web-mode-enable-css-colorization            t)
  (web-mode-enable-auto-pairing                nil)
  (web-mode-enable-current-column-highlight    t)
  :bind
  (:map web-mode-map
        (kbd "RET" . newline-and-indent)
        (kbd "M-RET" . newline)
                                                )
)

(provide 'web-mode-rcp)

;;; web-mode-rcp.el ends here
