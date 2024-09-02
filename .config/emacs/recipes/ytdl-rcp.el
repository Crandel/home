;;; ytdl-rcp.el --- Youtube-dl interface

;;; Code:
(use-package ytdl
  :ensure t
  :defer t
  :custom
  (ytdl-video-folder "/data/media/youtube")
  (ytdl-music-folder "/data/media/youtube/music")
  (ytdl-always-query-default-filename 'yes)
  (ytdl-video-extra-args '("-f bestvideo[height<=1080]+bestaudio"))
  (ytdl-music-extra-args '("-x bestaudio"))
)

(provide 'ytdl-rcp)
;;; Commentary:
;;
;;; ytdl-rcp.el ends here
