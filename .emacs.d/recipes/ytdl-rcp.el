;;; ytdl-rcp.el --- Youtube-dl interface

;;; Commentary:
;; 

;;; Code:

(use-package ytdl
  :ensure t
  :custom
  (ytdl-music-folder "/opt/work/backup/music")
  (ytdl-video-folder "/opt/work/backup/youtube")
  (ytdl-always-query-default-filename 'yes)
  :config
  (setq ytdl-video-extra-args '("bestvideo[height<=1080]" "bestaudio")
        ytdl-music-extra-args '("-x" "bestaudio"))
)

(provide 'ytdl-rcp)

;;; ytdl-rcp.el ends here
