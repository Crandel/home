;;; ytdl-rcp.el --- Youtube-dl interface

;;; Code:
(use-package ytdl
  :ensure t
  :defer t
  :custom
  (ytdl-video-folder "/data/backup/youtube")
  (ytdl-music-folder "/data/backup/youtube/music")
  (ytdl-always-query-default-filename 'yes)
  :config
  (setq ytdl-video-extra-args '("bestvideo[height<=1080]" "bestaudio")
        ytdl-music-extra-args '("-x" "bestaudio"))
)

(provide 'ytdl-rcp)
;;; Commentary:
;;
;;; ytdl-rcp.el ends here
