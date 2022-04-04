;;; svg-tag-mode-rcp.el --- A minor mode to replace keywords or regular expression with SVG tags.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package svg-tag-mode
  :ensure t
  :preface
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  ;; :custom
  ;; (svg-tag-tags '((":TODO:" . ((svg-tag-make "TODO" :face 'org-tag
  ;;                                            :radius 0 :inverse t :margin 0)))
                  ;; ("\\/\\/\\W?TODO\\b\\|TODO\\b" . ((svg-tag-make "TODO" :face 'org-tag
                                             ;; :radius 0 :inverse t :margin 0)))
                  ;; ("TODO\\b\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'org-todo :crop-left t))))
                  ;; (":NOTE:" . ((svg-tag-make "NOTE" :face 'font-lock-comment-face
                  ;;                            :inverse nil :margin 0 :radius 0)))
                  ;; ("\([0-9a-zA-Z]\)" . ((lambda (tag)
                  ;;                         (svg-tag-make tag :beg 1 :end -1 :radius 12))))
                  ;; ("\([0-9a-zA-Z][0-9a-zA-Z]\)" . ((lambda (tag)
                  ;;                                    (svg-tag-make tag :beg 1 :end -1 :radius 8))))
                  ;; ("|[0-9a-zA-Z- ]+?|" . ((lambda (tag)
                  ;;                           (svg-tag-make tag :face 'font-lock-comment-face
                  ;;                                         :margin 0 :beg 1 :end -1))))
                  ;; (,(format "\\(<%s>\\)" date-re) .
                  ;;  ((lambda (tag)
                  ;;     (svg-tag-make tag :beg 1 :end -1 :margin 0))))
                  ;; (,(format "\\(<%s *\\)%s>" date-re time-re) .
                  ;;  ((lambda (tag)
                  ;;     (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
                  ;; (,(format "<%s *\\(%s>\\)" date-re time-re) .
                  ;;  ((lambda (tag)
                  ;;     (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

                  ;; ;; Inactive date  (without day name, with or without time)
                  ;; (,(format "\\(\\[%s\\]\\)" date-re) .
                  ;;  ((lambda (tag)
                  ;;     (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
                  ;; (,(format "\\(\\[%s *\\)%s\\]" date-re time-re) .
                  ;;  ((lambda (tag)
                  ;;     (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
                  ;; (,(format "\\[%s *\\(%s\\]\\)" date-re time-re) .
                  ;;  ((lambda (tag)
                  ;;     (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))
                  ;; ))
  :hook ((prog-mode . svg-tag-mode)
         (org-mode . svg-tag-mode))
  )

(provide 'svg-tag-mode-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; svg-tag-mode-rcp.el ends here
