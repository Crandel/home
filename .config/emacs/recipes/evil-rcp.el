;;; evil-rcp.el --- Evil mode

;;; Code:
(use-package evil
  :ensure t
  :defer t
  :custom
  (evil-normal-state-tag   (propertize " <N> " 'face '((:background "DarkGoldenrod2" :foreground "black"))))
  (evil-emacs-state-tag    (propertize " <E> " 'face '((:background "SkyBlue2"       :foreground "black"))))
  (evil-insert-state-tag   (propertize " <I> " 'face '((:background "chartreuse3"    :foreground "black"))))
  (evil-replace-state-tag  (propertize " <R> " 'face '((:background "chocolate"      :foreground "black"))))
  (evil-motion-state-tag   (propertize " <M> " 'face '((:background "plum3"          :foreground "black"))))
  (evil-visual-state-tag   (propertize " <V> " 'face '((:background "gray"           :foreground "black"))))
  (evil-operator-state-tag (propertize " <O> " 'face '((:background "sandy brown"    :foreground "black"))))
  :bind (("C-x e" . evil-mode)
         :map evil-normal-state-map
         ("C-p" . helm-multi-files)))

(use-package evil-mc
  :ensure t
  :defer t
  :hook
  (evil-mode . evil-mc-mode)
)

(provide 'evil-rcp)
;;; Commentary:
;;
;;; evil-rcp.el ends here
