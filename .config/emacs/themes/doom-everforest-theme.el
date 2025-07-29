;;; doom-everforest-theme.el --- inspired by Everforest
;;; https://github.com/sainnhe/everforest
(require 'doom-themes)

;;
(defgroup doom-everforest-theme nil
  "Options for doom-everforest"
  :group 'doom-themes)

(defcustom doom-everforest-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-everforest-theme
  :type 'boolean)

(defcustom doom-everforest-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-everforest-theme
  :type 'boolean)

(defcustom doom-everforest-comment-bg doom-everforest-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-everforest-theme
  :type 'boolean)

(defcustom doom-everforest-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-everforest-theme
  :type '(choice integer boolean))

(defcustom doom-everforest-background nil
  "Choice between \"soft\", \"medium\" and \"hard\" background contrast.
Defaults to \"soft\""
  :group 'doom-everforest-theme
  :type 'string)

(defcustom doom-everforest-palette nil
  "Choose between \"material\", \"mix\" and \"original\" color palette.
Defaults to \"material\""
  :group 'doom-everforest-theme
  :type 'string)

(defcustom doom-everforest-dired-height 1.15
  "Font height for dired buffers"
  :group 'doom-everforest-theme
  :type 'float)
;; colors from
;; https://github.com/sainnhe/everforest/blob/master/autoload/everforest.vim
(cond
 ((equal doom-everforest-background "hard")
  (setq ef/bg           "#272e33"       ;; bg0
        ef/bg-alt       "#1e2326"       ;; bg1
        ef/base0        "#3a454a"       ;; bg2
        ef/base1        "#445055"       ;; bg3
        ef/base2        "#4c555b"       ;; bg4
        ef/base3        "#53605c"       ;; bg5
        ef/base4        "#503946"))       ;; bg_visual
 ((equal doom-everforest-background "medium")
  (setq ef/bg           "#2d353b"       ;; bg0
        ef/bg-alt       "#232a2e"       ;; bg1
        ef/base0        "#404c51"       ;; bg2
        ef/base1        "#4a555b"       ;; bg3
        ef/base2        "#525c62"       ;; bg4
        ef/base3        "#596763"       ;; bg5
        ef/base4        "#573e4c"))     ;; bg_visual
 (t
  (setq ef/bg           "#333c43"       ;; bg0
        ef/bg-alt       "#293136"       ;; bg1
        ef/base0        "#465258"       ;; bg2
        ef/base1        "#505a60"       ;; bg3
        ef/base2        "#576268"       ;; bg4
        ef/base3        "#5f6d67"       ;; bg5
        ef/base4        "#5d4251")))    ;; bg_visual

(def-doom-theme doom-everforest
  "A dark theme inspired by Everforest"
  ;; name        default        256       16
  ((bg         `(,ef/bg         nil       nil            ))
   (bg-alt     `(,ef/bg-alt     nil       nil            ))
   (base0      `(,ef/base0      "black"   "black"        ))
   (base1      `(,ef/base1      "#1e1e1e" "brightblack"  ))
   (base2      `(,ef/base2      "#2e2e2e" "brightblack"  ))
   (base3      `(,ef/base3      "#262626" "brightblack"  ))
   (base4      `(,ef/base4      "#3f3f3f" "brightblack"  ))
   (base5      '("#7a8478"       "#525252" "brightblack"  ))
   (base6      '("#859289"       "#6b6b6b" "brightblack"  ))
   (base7      '("#9da9a0"       "#979797" "brightblack"  ))
   (base8      '("#7a8478"       "#dfdfdf" "white"        ))
   (fg         '("#d3c6aa"      "#bfbfbf" "brightwhite"  ))
   (fg-alt     '("#b9c0ab"      "#2d2d2d" "white"        )) ;; bg5 light soft

   (grey       base8)
   (red        '("#e67e80"      "#ff6655" "red"          ))
   (orange     '("#e69875"      "#dd8844" "brightred"    ))
   (green      '("#a7c080"      "#99bb66" "green"        ))
   (teal       '("#83c092"      "#44b9b1" "brightgreen"  )) ;; aqua
   (yellow     '("#dbbc7f"      "#ECBE7B" "yellow"       ))
   (blue       '("#7fbbb3"      "#51afef" "brightblue"   ))
   (dark-blue  `("#60948d"      "#2257A0" "blue"         )) ;; own
   (magenta    '("#d699b6"      "#c678dd" "brightmagenta")) ;; purple
   (violet     '("#d699b6"      "#a9a1e1" "magenta"      )) ;; purple
   (cyan       '("#83c092"      "#46D9FF" "brightcyan"   )) ;; aqua
   (dark-cyan  `("#74ab82"      "#5699AF" "cyan"         )) ;; own

   ;; face categories -- required for all themes
   (highlight      cyan)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if doom-everforest-brighter-comments cyan
                     (doom-blend magenta cyan 0.65)))
   (doc-comments   (doom-lighten (if doom-everforest-brighter-comments dark-cyan green) 0.2))
   (constants      violet)
   (functions      cyan)
   (keywords       (doom-lighten teal 0.1))
   (methods        cyan)
   (operators      blue)
   (type           orange)
   (strings        green)
   (variables      blue)
   (numbers        magenta)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-everforest-brighter-modeline)
   (-modeline-pad
    (when doom-everforest-padded-modeline
      (if (integerp doom-everforest-padded-modeline) doom-everforest-padded-modeline 4)))

   (modeline-fg     fg)
   (modeline-fg-alt fg-alt)

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.475)
      `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg-alt)))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground base5)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-everforest-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; Doom modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)

   ;; ivy-mode
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)

   ;; --- major-mode faces -------------------
   ;; column indicator
   (fill-column-indicator :foreground bg-alt :background bg-alt)

   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; cursor
   (cursor :foreground fg :background blue)

   ;; dired
   (diredfl-compressed-file-name :height doom-everforest-dired-height
                    :foreground yellow)
   (diredfl-dir-heading :height doom-everforest-dired-height
                        :foreground teal)
   (diredfl-dir-name :height doom-everforest-dired-height
                     :foreground blue)
   (diredfl-deletion :height doom-everforest-dired-height
                     :foreground red :background (doom-lighten red 0.55))
   (diredfl-deletion-file-name :foreground red
                               :background (doom-lighten red 0.55))
   (diredfl-file-name :height doom-everforest-dired-height
                      :foreground fg)
   (dired-flagged :height doom-everforest-dired-height
                    :foreground red :background (doom-lighten red 0.55))
   (diredfl-symlink :height doom-everforest-dired-height
                    :foreground magenta)

   ;; ein
   (ein:basecell-input-area-face :background bg)

   ;; eshell
   (+eshell-prompt-git-branch :foreground cyan)

   ;; evil
   (evil-ex-lazy-highlight :foreground fg :background (doom-darken orange 0.3))
   (evil-snipe-first-match-face :foreground bg :background orange)


   ;; ivy
   (ivy-current-match :foreground blue :background bg)
   (ivy-minibuffer-match-face-2 :foreground blue :background bg)

   ;; LaTeX-mode
   (font-latex-math-face :foreground (doom-lighten green 0.15))
   (font-latex-script-char-face :foreground (doom-lighten dark-blue 0.15))

   ;; lsp
   (lsp-face-highlight-read :foreground fg-alt
                           :background (doom-darken dark-blue 0.3))
   (lsp-face-highlight-textual :foreground fg-alt
                           :background (doom-lighten dark-blue 0.3))
   (lsp-face-highlight-write :foreground fg-alt
                             :background (doom-darken dark-blue 0.3))
   (lsp-lsp-flycheck-info-unnecessary-face :foreground (doom-lighten yellow 0.12))

   ;; magit
   (magit-section-heading :foreground blue :weight 'bold)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))

   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden)
   (org-drawer :foreground (doom-darken yellow 0.15))
   (org-document-info :foreground blue)
   (org-document-info-keyword :foreground dark-blue)
   (org-document-title :foreground blue)
   (org-block-begin-line :foreground dark-cyan
                         :background bg-alt)
   (org-block-end-line :foreground dark-cyan
                         :background bg-alt)
   (org-block :foreground fg :background bg-alt)
   (org-meta-line :foreground dark-cyan)
   (org-level-1 :foreground magenta :weight 'semi-bold :height 1.4)
   (org-level-2 :foreground cyan :weight 'semi-bold :height 1.2)
   (org-level-3 :foreground green :weight 'semi-bold :height 1.1)
   (org-level-4 :foreground yellow :weight 'semi-bold)
   (org-level-5 :foreground violet :weight 'semi-bold)
   (org-level-6 :foreground dark-cyan :weight 'semi-bold)
   (org-level-7 :foreground (doom-darken green 0.15) :weight 'semi-bold)
   (org-level-8 :foreground (doom-darken yellow 0.15) :weight 'semi-bold)

   ;; org-ref
   (org-ref-ref-face :foreground magenta)

   ;; org-roam
   (org-roam-title :foreground orange :weight 'semi-bold)

   ;; rainbow and parenthesis
   (rainbow-delimiters-depth-1-face :foreground orange)
   (rainbow-delimiters-depth-2-face :foreground violet)
   (rainbow-delimiters-depth-3-face :foreground dark-cyan)
   (rainbow-delimiters-depth-4-face :foreground (doom-darken yellow 0.15))
   (rainbow-delimiters-unmatched-face: :foreground fg :background 'nil)
   (show-paren-match :foreground bg :background (doom-darken red 0.15))

   ;; vertico
   (vertico-current :foreground fg :background (doom-lighten bg 0.1))

   ;; others
   (isearch :foreground fg :background violet)
   (selection :foreground bg-alt :background (doom-darken orange 0.15))
   (company-tooltip-common-selection :foreground bg-alt :background dark-blue)
   )


  ;; --- extra variables ---------------------
  ()
  )

;;; doom-everforest-theme.el ends here
