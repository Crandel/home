;;; consult-rcp.el --- Consult provides various practical commands based on the Emacs completion function completing-read,
;;; which allows to quickly select an item from a list of candidates with completion.

;;; Code:
(use-package consult
  :ensure t
  :bind
  ("C-s" . consult-line)
  ("C-x g" . consult-ripgrep)
  ("C-c b" . consult-buffer)
  ("C-x C-b" . consult-buffer)
  :chords
  ("bl" . consult-buffer)
)

(provide 'consult-rcp)

;;; Commentary:
;;
;;; consult-rcp.el ends here
