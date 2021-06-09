;;; org-jira-rcp.el --- Use Jira in Emacs org-mode.
;;; Code:
(use-package org-jira
  :ensure t
  :defer t
  :commands (org-jira-mode org-jira-get-issues)
  :custom
  (jiralib-url "https://hitmeister.atlassian.net")
)

(provide 'org-jira-rcp)
;;; Commentary:
;;
;;; org-jira-rcp.el ends here
