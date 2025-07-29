;;; mail-rcp.el --- Email setup for Emacs.
;;; Code:
(eval-when-compile (require 'use-package))
(use-package gnus
  :custom
  (gnus-always-read-dribble-file t)
  (gnus-select-method             '(nntp "news.gmane.io"))
  (gnus-secondary-select-methods  '((nntp "news.gwene.org")))
  (gnus-check-new-newsgroups      'ask-server)
  (gnus-read-active-file          'some)
  (gnus-search-use-parsed-queries nil)
  :config
  (require 'gnus-sum)
  (require 'gnus-dired)
  (require 'gnus-topic)

)
(provide 'mail-rcp)

;;; Commentary:
;;
;;; mail-rcp.el ends here
