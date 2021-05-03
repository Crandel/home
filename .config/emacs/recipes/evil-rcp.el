;;; evil-rcp.el --- Evil mode

;;; Code:
(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-normal-state-tag   (propertize "<N>" 'face '((:background "DarkGoldenrod2" :foreground "black")))
        evil-emacs-state-tag    (propertize "<E>" 'face '((:background "SkyBlue2"       :foreground "black")))
        evil-insert-state-tag   (propertize "<I>" 'face '((:background "chartreuse3"    :foreground "black")))
        evil-replace-state-tag  (propertize "<R>" 'face '((:background "chocolate"      :foreground "black")))
        evil-motion-state-tag   (propertize "<M>" 'face '((:background "plum3"          :foreground "black")))
        evil-visual-state-tag   (propertize "<V>" 'face '((:background "gray"           :foreground "black")))
        evil-operator-state-tag (propertize "<O>" 'face '((:background "sandy brown"    :foreground "black")))
        evil-undo-system        'undo-tree
        evil-want-fine-undo     t)
  :config
  (evil-mode 1)
  :hook
  (evil-local-mode-hook turn-on-undo-tree-mode)
  :bind (("C-x e" . evil-mode)
         ("M-," . lsp-ui-peek-find-references)
         :map evil-normal-state-map
         ("M-." . lsp-ui-peek-find-definitions)
         ("q" . nil)
         ("qq" . delete-other-windows)
         ("C-p" . helm-multi-files)
         :map evil-insert-state-map
         ("C-p" . helm-multi-files)
         )
  )

(use-package evil-mc
  :ensure t
  :after evil-mode
  :hook
  (evil-local-mode . evil-mc-mode)
  :bind (
         :map evil-mc-key-map
         ("C-p" . helm-multi-files)
         ("C-c C-<right>" . 'evil-mc-make-and-goto-next-match) ; choose same word next
         ("C-c C-<left>" . 'evil-mc-make-and-goto-prev-match) ; choose same word previous
         ("M-n" . 'evil-mc-make-and-goto-next-match) ; choose char from next line same position
         ("M-m" . 'evil-mc-make-and-goto-prev-match); choose char from previous line same position
         ("C-c C-_" . 'evil-mc-make-all-cursors)
         ("C-x M-m" . 'back-to-indentation)
         ("C-c C-n" . 'evil-mc-skip-and-goto-next-match)
         ("C-c <return>" . 'evil-mc-skip-and-goto-prev-match)
         ("ESC" . 'evil-mc-undo-all-cursors)
         )
)

(use-package evil-surround
  :ensure t
  :custom
  (evil-surround-pairs-alist
   '((40 "(" . ")")
     (91 "[" . "]")
     (123 "{" . "}")
     (41 "(" . ")")
     (93 "[" . "]")
     (125 "{" . "}")
     (35 "#{" . "}")
     (98 "(" . ")")
     (66 "{" . "}")
     (62 "<" . ">")
     (116 . evil-surround-read-tag)
     (60 . evil-surround-read-tag)
     (102 . evil-surround-function)))
  :config
  (global-evil-surround-mode 1))

(provide 'evil-rcp)
;;; Commentary:
;;
;;; evil-rcp.el ends here
