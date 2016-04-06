;; Company faces
(custom-set-faces
 '(company-preview-common ((t (:inherit company-preview :foreground "brightcyan"))))
 '(company-preview-search ((t (:inherit company-preview :background "black"))))
 '(company-scrollbar-bg ((t (:background "color-16"))))
 '(company-scrollbar-fg ((t (:background "brightyellow"))))
 '(company-template-field ((t (:background "black" :foreground "yellow"))))
 '(company-tooltip ((t (:background "black" :foreground "brightyellow"))))
 '(company-tooltip-selection ((t (:background "color-108")))))

;; Paren face
(set-face-background 'show-paren-match (face-background 'default))
    (set-face-foreground 'show-paren-match "#def")
    (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
(provide 'my_faces)
