;;; hydra-rcp.el ---     Make bindings that stick around.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package hydra
  :ensure t
  :config
  (defhydra vd-mc-hydra (:foreign-keys run
                         :hint nil
                         :pre (evil-mc-pause-cursors))
   "
   ^Match^            ^Line-wise^           ^Manual^
   ^^^^^^--------------------------------------
   _Z_: match all     _J_: make & go down   _z_: toggle cursor here
   _m_: make & next   _K_: make & go up     _r_: remove last
   _M_: make & prev   _]_: goto next cursor _R_: remove all
   _n_: skip & next   _[_: goto prev cursor _q_: quit
   _N_: skip & prev

   Current pattern: %`evil-mc-pattern
   "
    ("Z" #'evil-mc-make-all-cursors)
    ("m" #'evil-mc-make-and-goto-next-match)
    ("M" #'evil-mc-make-and-goto-prev-match)
    ("n" #'evil-mc-skip-and-goto-next-match)
    ("N" #'evil-mc-skip-and-goto-prev-match)
    ("]" #'evil-mc-make-and-goto-next-cursor)
    ("[" #'evil-mc-make-and-goto-prev-cursor)
    ("z" #'evil-mc-make-cursor-here)
    ("J" #'evil-mc-make-cursor-move-next-line)
    ("K" #'evil-mc-make-cursor-move-prev-line)
    ("r" #'evil-mc-undo-last-added-cursor)
    ("R" #'evil-mc-undo-all-cursors)
    ("q" #'evil-mc-resume-cursors "quit" :exit t)
    ("<escape>" #'evil-mc-resume-cursors "quit" :exit t)
  )
  :bind(
   :map evil-normal-state-map
   ("m" . vd-mc-hydra/body)
   :map evil-visual-state-map
   ("m" . vd-mc-hydra/body)
  )
)

(provide 'hydra-rcp)
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; hydra-rcp.el ends here
