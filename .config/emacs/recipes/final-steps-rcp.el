;;; final-steps-rcp.el --- Final steps after all packages were loaded

;;; Code:
;;;###autoload
(defun vd/tab-indent-or-complete ()
  "Function to choose correct TAB action."
  (interactive)
  (cond
   ((derived-mode-p 'org-mode) (org-cycle))
   ((derived-mode-p 'minibuffer-mode)
    (when (fboundp 'vertico-next)
      (vertico-next)))
   (t (progn
        (when (fboundp 'tempel-expand)
          (tempel-expand))
        (indent-for-tab-command)))))

(define-key global-map (kbd "TAB") 'vd/tab-indent-or-complete)
(define-key minibuffer-mode-map (kbd "C-c u") 'minibuffer-complete)
(cl-loop
 for from across "йцукенгшщзхїфівапролджєячсмитьбюЙЦУКЕНГШЩЗХЇФІВАПРОЛДЖ\ЄЯЧСМИТЬБЮ№"
 for to   across "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"
 do
 (eval `(define-key local-function-key-map
          (kbd ,(concat "C-"
                  (string from)))
          (kbd ,(concat "C-"
                  (string to)))))
 (eval `(define-key local-function-key-map
          (kbd ,(concat "M-"
                  (string from)))
          (kbd ,(concat "M-"
                  (string to)))))
 (eval `(define-key local-function-key-map
          (kbd ,(string from))
          (kbd ,(string to)))))

(provide 'final-steps-rcp)
;;; Commentary:
;;
;;; final-steps-rcp.el ends here