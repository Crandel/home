;;; gruvbox-theme.el --- A retro-groove colour theme for Emacs

;; Copyright (c) 2013 Lee Machin
;; Copyright (c) 2013-2016 Greduan

;; Authors: Lee Machin <ljmachin@gmail.com>
;;          Greduan <me@greduan.com>
;; Maintainer: jasonm23 <jasonm23@gmail.com>
;; URL: http://github.com/Greduan/emacs-theme-gruvbox
;; Version: 0.18

;;; Commentary:

;; A port of the Gruvbox colorscheme for Vim, built on top of the new built-in
;; theme support in Emacs 24.
;;
;; This theme contains my own modifications and it's a bit opinionated
;; sometimes, deviating from the original because of it. I try to stay
;; true to the original as much as possible, however. I only make
;; changes where I would have made the changes on the original.
;;
;; Since there is no direct equivalent in syntax highlighting from Vim to Emacs
;; some stuff may look different, especially in stuff like JS2-mode, where it
;; adds stuff that Vim doesn't have, in terms of syntax.

;;; Credits:

;; Pavel Pertsev created the original theme for Vim, on which this port
;; is based.

;; Lee Machin created the first port of the original theme, which I'm working on
;; to make better and more feature complete.

;;; Code:
(eval-when-compile
  (require 'cl-lib))

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))

(defcustom gruvbox-contrast 'medium
  "Contrast level for the theme background."
  :options '(soft medium hard))

(deftheme gruvbox "A retro-groove colour theme")
(let* ((gruvbox-dark0_hard  (if (display-graphic-p) "#1d2021" "color-234"))
      (gruvbox-dark0       (if (display-graphic-p) "#282828" "color-235"))
      (gruvbox-dark0_soft  (if (display-graphic-p) "#32302f" "color-236"))
      (gruvbox-dark1       (if (display-graphic-p) "#3c3836" "color-237"))
      (gruvbox-dark2       (if (display-graphic-p) "#504945" "color-239"))
      (gruvbox-dark3       (if (display-graphic-p) "#665c54" "color-241"))
      (gruvbox-dark4       (if (display-graphic-p) "#7c6f64" "color-243"))

      (gruvbox-medium      (if (display-graphic-p) "#928374" "color-245")) ;; or 244

      (gruvbox-light0_hard (if (display-graphic-p) "#f9f5d7" "color-230"))
      (gruvbox-light0      (if (display-graphic-p) "#fbf1c7" "color-229"))
      (gruvbox-light0_soft (if (display-graphic-p) "#f2e5bc" "color-228"))
      (gruvbox-light1      (if (display-graphic-p) "#ebdbb2" "color-223"))
      (gruvbox-light2      (if (display-graphic-p) "#d5c4a1" "color-250"))
      (gruvbox-light3      (if (display-graphic-p) "#bdae93" "color-248"))
      (gruvbox-light4      (if (display-graphic-p) "#a89984" "color-246"))

      (gruvbox-bright_red     (if (display-graphic-p) "#fb4934" "color-167"))
      (gruvbox-bright_green   (if (display-graphic-p) "#b8bb26" "color-142"))
      (gruvbox-bright_yellow  (if (display-graphic-p) "#fabd2f" "color-214"))
      (gruvbox-bright_blue    (if (display-graphic-p) "#83a598" "color-109"))
      (gruvbox-bright_purple  (if (display-graphic-p) "#d3869b" "color-175"))
      (gruvbox-bright_aqua    (if (display-graphic-p) "#8ec07c" "color-108"))
      (gruvbox-bright_orange  (if (display-graphic-p) "#fe8019" "color-208"))

      ;; neutral, no 256-color code, requested, nice work-around meanwhile
      (gruvbox-neutral_red    (if (display-graphic-p) "#cc241d" "#d75f5f"))
      (gruvbox-neutral_green  (if (display-graphic-p) "#98971a" "#afaf00"))
      (gruvbox-neutral_yellow (if (display-graphic-p) "#d79921" "#ffaf00"))
      (gruvbox-neutral_blue   (if (display-graphic-p) "#458588" "#87afaf"))
      (gruvbox-neutral_purple (if (display-graphic-p) "#b16286" "#d787af"))
      (gruvbox-neutral_aqua   (if (display-graphic-p) "#689d6a" "#87af87"))
      (gruvbox-neutral_orange (if (display-graphic-p) "#d65d0e" "#ff8700"))

      (gruvbox-faded_red      (if (display-graphic-p) "#9d0006" "color-88"))
      (gruvbox-faded_green    (if (display-graphic-p) "#79740e" "color-100"))
      (gruvbox-faded_yellow   (if (display-graphic-p) "#b57614" "color-136"))
      (gruvbox-faded_blue     (if (display-graphic-p) "#076678" "color-24"))
      (gruvbox-faded_purple   (if (display-graphic-p) "#8f3f71" "color-96"))
      (gruvbox-faded_aqua     (if (display-graphic-p) "#427b58" "color-66"))
      (gruvbox-faded_orange   (if (display-graphic-p) "#af3a03" "color-130"))

      (gruvbox-dark_red        (if (display-graphic-p) "#421E1E" "color-52"))
      (gruvbox-dark_blue       (if (display-graphic-p) "#2B3C44" "color-4"))
      (gruvbox-dark_aqua       (if (display-graphic-p) "#36473A" "color-23"))

      (gruvbox-delimiter-one    (if (display-graphic-p) "#458588" "color-30"))
      (gruvbox-delimiter-two    (if (display-graphic-p) "#b16286" "color-168"))
      (gruvbox-delimiter-three  (if (display-graphic-p) "#8ec07c" "color-108"))
      (gruvbox-delimiter-four   (if (display-graphic-p) "#d65d0e" "color-166"))
      (gruvbox-white            (if (display-graphic-p) "#FFFFFF" "white"))
      (gruvbox-black            (if (display-graphic-p) "#000000" "black"))
      (gruvbox-sienna           (if (display-graphic-p) "#DD6F48" "sienna"))
      (gruvbox-darkslategray4   (if (display-graphic-p) "#528B8B" "DarkSlateGray4"))
      (gruvbox-lightblue4       (if (display-graphic-p) "#66999D" "LightBlue4"))
      (gruvbox-burlywood4       (if (display-graphic-p) "#BBAA97" "burlywood4"))
      (gruvbox-aquamarine4      (if (display-graphic-p) "#83A598" "aquamarine4"))
      (gruvbox-turquoise4       (if (display-graphic-p) "#61ACBB" "turquoise4"))

      (gruvbox-bg (cl-case gruvbox-contrast
        (hard gruvbox-dark0_hard)
        (soft gruvbox-dark0_soft)
        ;; Medium by default.
        (t    gruvbox-dark0))))
  (custom-theme-set-faces
    'gruvbox

    ;; UI
    `(default                           ((t (:background ,gruvbox-bg :foreground ,gruvbox-light0))))
    `(cursor                            ((t (:background ,gruvbox-light0))))
    `(mode-line                         ((t (:box nil :background ,gruvbox-dark2 :foreground ,gruvbox-light2))))
    `(mode-line-inactive                ((t (:box nil :background ,gruvbox-dark1 :foreground ,gruvbox-light4))))
    `(fringe                            ((t (:background ,gruvbox-bg))))
    `(linum                             ((t (:background ,gruvbox-bg :foreground ,gruvbox-dark4))))
    `(hl-line                           ((t (:background ,gruvbox-dark1))))
    `(region                            ((t (:background ,gruvbox-dark2)))) ;;selection
    `(secondary-selection               ((t (:background ,gruvbox-dark1))))
    `(minibuffer-prompt                 ((t (:background ,gruvbox-bg :foreground ,gruvbox-neutral_green :bold t))))
    `(vertical-border                   ((t (:foreground ,gruvbox-dark2))))
    `(link                              ((t (:foreground ,gruvbox-faded_blue :underline t))))
    `(shadow                            ((t (:foreground ,gruvbox-dark4))))

    ;; Built-in syntax
    `(font-lock-builtin-face            ((t (:foreground ,gruvbox-neutral_orange))))
    `(font-lock-constant-face           ((t (:foreground ,gruvbox-neutral_purple))))
    `(font-lock-comment-face            ((t (:foreground ,gruvbox-dark4))))
    `(font-lock-function-name-face      ((t (:foreground ,gruvbox-neutral_yellow))))
    `(font-lock-keyword-face            ((t (:foreground ,gruvbox-neutral_green))))
    `(font-lock-string-face             ((t (:foreground ,gruvbox-neutral_aqua))))
    `(font-lock-variable-name-face      ((t (:foreground ,gruvbox-neutral_blue))))
    `(font-lock-type-face               ((t (:foreground ,gruvbox-neutral_purple))))
    `(font-lock-warning-face            ((t (:foreground ,gruvbox-neutral_red :bold t))))

    ;; whitespace-mode
    `(whitespace-space                  ((t (:background ,gruvbox-bg :foreground ,gruvbox-dark4))))
    `(whitespace-hspace                 ((t (:background ,gruvbox-bg :foreground ,gruvbox-dark4))))
    `(whitespace-tab                    ((t (:background ,gruvbox-bg :foreground ,gruvbox-dark4))))
    `(whitespace-newline                ((t (:background ,gruvbox-bg :foreground ,gruvbox-dark4))))
    `(whitespace-trailing               ((t (:background ,gruvbox-dark1 :foreground ,gruvbox-neutral_red))))
    `(whitespace-line                   ((t (:background ,gruvbox-dark1 :foreground ,gruvbox-neutral_red))))
    `(whitespace-space-before-tab       ((t (:background ,gruvbox-bg :foreground ,gruvbox-dark4))))
    `(whitespace-indentation            ((t (:background ,gruvbox-bg :foreground ,gruvbox-dark4))))
    `(whitespace-empty                  ((t (:background nil :foreground nil))))
    `(whitespace-space-after-tab        ((t (:background ,gruvbox-bg :foreground ,gruvbox-dark4))))

    ;; RainbowDelimiters
    `(rainbow-delimiters-depth-1-face   ((t (:foreground ,gruvbox-delimiter-one))))
    `(rainbow-delimiters-depth-2-face   ((t (:foreground ,gruvbox-delimiter-two))))
    `(rainbow-delimiters-depth-3-face   ((t (:foreground ,gruvbox-delimiter-three))))
    `(rainbow-delimiters-depth-4-face   ((t (:foreground ,gruvbox-delimiter-four))))
    `(rainbow-delimiters-depth-5-face   ((t (:foreground ,gruvbox-delimiter-one))))
    `(rainbow-delimiters-depth-6-face   ((t (:foreground ,gruvbox-delimiter-two))))
    `(rainbow-delimiters-depth-7-face   ((t (:foreground ,gruvbox-delimiter-three))))
    `(rainbow-delimiters-depth-8-face   ((t (:foreground ,gruvbox-delimiter-four))))
    `(rainbow-delimiters-depth-9-face   ((t (:foreground ,gruvbox-delimiter-one))))
    `(rainbow-delimiters-depth-10-face  ((t (:foreground ,gruvbox-delimiter-two))))
    `(rainbow-delimiters-depth-11-face  ((t (:foreground ,gruvbox-delimiter-three))))
    `(rainbow-delimiters-depth-12-face  ((t (:foreground ,gruvbox-delimiter-four))))
    `(rainbow-delimiters-unmatched-face ((t (:background nil :foreground ,gruvbox-light0))))

    ;; linum-relative
    `(linum-relative-current-face       ((t (:background ,gruvbox-dark1 :foreground ,gruvbox-light4))))

    ;; Highlight indentation mode
    `(highlight-indentation-current-column-face ((t (:background ,gruvbox-dark2 ))))
    `(highlight-indentation-face                ((t (:background ,gruvbox-dark1 ))))

    ;; Smartparens
    `(sp-pair-overlay-face              ((t (:background ,gruvbox-dark2))))
    ;`(sp-wrap-overlay-face             ((t (:inherit sp-wrap-overlay-face))))
    ;`(sp-wrap-tag-overlay-face         ((t (:inherit sp-wrap-overlay-face))))
    `(sp-show-pair-match-face           ((t (:background ,gruvbox-dark2)))) ;; Pair tags highlight
    `(sp-show-pair-mismatch-face        ((t (:background ,gruvbox-neutral_red)))) ;; Highlight for bracket without pair

    ;; elscreen
    `(elscreen-tab-background-face      ((t (:box nil :background ,gruvbox-bg)))) ;; Tab bar, not the tabs
    `(elscreen-tab-control-face         ((t (:box nil :background ,gruvbox-dark2 :foreground ,gruvbox-neutral_red :underline nil)))) ;; The controls
    `(elscreen-tab-current-screen-face  ((t (:box nil :background ,gruvbox-dark4 :foreground ,gruvbox-dark0)))) ;; Current tab
    `(elscreen-tab-other-screen-face    ((t (:box nil :background ,gruvbox-dark2 :foreground ,gruvbox-light4 :underline nil)))) ;; Inactive tab

    ;; ag (The Silver Searcher)
    `(ag-hit-face                       ((t (:foreground ,gruvbox-neutral_blue))))
    `(ag-match-face                     ((t (:foreground ,gruvbox-neutral_red))))

    ;; Diffs
    `(diff-changed                      ((t (:background nil :foreground ,gruvbox-light1))))
    `(diff-added                        ((t (:background nil :foreground ,gruvbox-neutral_green))))
    `(diff-removed                      ((t (:background nil :foreground ,gruvbox-neutral_red))))
    `(diff-indicator-changed            ((t (:inherit diff-changed))))
    `(diff-indicator-added              ((t (:inherit diff-added))))
    `(diff-indicator-removed            ((t (:inherit diff-removed))))

    `(js2-warning                       ((t (:underline (:color ,gruvbox-bright_yellow :style wave)))))
    `(js2-error                         ((t (:underline (:color ,gruvbox-bright_red :style wave)))))
    `(js2-external-variable             ((t (:underline (:color ,gruvbox-bright_aqua :style wave)))))
    `(js2-jsdoc-tag                     ((t (:background nil :foreground ,gruvbox-medium ))))
    `(js2-jsdoc-type                    ((t (:background nil :foreground ,gruvbox-light4 ))))
    `(js2-jsdoc-value                   ((t (:background nil :foreground ,gruvbox-light3 ))))
    `(js2-function-param                ((t (:background nil :foreground ,gruvbox-bright_aqua ))))
    `(js2-function-call                 ((t (:background nil :foreground ,gruvbox-bright_blue ))))
    `(js2-instance-member               ((t (:background nil :foreground ,gruvbox-bright_orange ))))
    `(js2-private-member                ((t (:background nil :foreground ,gruvbox-faded_yellow ))))
    `(js2-private-function-call         ((t (:background nil :foreground ,gruvbox-faded_aqua ))))
    `(js2-jsdoc-html-tag-name           ((t (:background nil :foreground ,gruvbox-light4 ))))
    `(js2-jsdoc-html-tag-delimiter      ((t (:background nil :foreground ,gruvbox-light3 ))))


    ;; popup
    `(popup-face                                ((t (:foreground ,gruvbox-light1 :background ,gruvbox-dark1))))
    `(popup-menu-mouse-face                     ((t (:foreground ,gruvbox-light0 :background ,gruvbox-faded_green))))
    `(popup-menu-selection-face                 ((t (:foreground ,gruvbox-light0 :background ,gruvbox-faded_green))))
    `(popup-tip-face                            ((t (:foreground ,gruvbox-light2 :background ,gruvbox-dark2))))


    ;; helm
    `(helm-M-x-key                              ((t ( :foreground ,gruvbox-neutral_orange  ))))
    `(helm-action                               ((t ( :foreground ,gruvbox-white :underline t ))))
    `(helm-bookmark-addressbook                 ((t ( :foreground ,gruvbox-neutral_red ))))
    `(helm-bookmark-directory                   ((t ( :foreground ,gruvbox-bright_purple ))))
    `(helm-bookmark-file                        ((t ( :foreground ,gruvbox-faded_blue ))))
    `(helm-bookmark-gnus                        ((t ( :foreground ,gruvbox-faded_purple ))))
    `(helm-bookmark-info                        ((t ( :foreground ,gruvbox-turquoise4 ))))
    `(helm-bookmark-man                         ((t ( :foreground ,gruvbox-sienna ))))
    `(helm-bookmark-w3m                         ((t ( :foreground ,gruvbox-neutral_yellow ))))
    `(helm-buffer-directory                     ((t ( :foreground ,gruvbox-white         :background ,gruvbox-bright_blue  ))))
    `(helm-buffer-not-saved                     ((t ( :foreground ,gruvbox-faded_red ))))
    `(helm-buffer-process                       ((t ( :foreground ,gruvbox-burlywood4 ))))
    `(helm-buffer-saved-out                     ((t ( :foreground ,gruvbox-bright_red ))))
    `(helm-buffer-size                          ((t ( :foreground ,gruvbox-bright_purple ))))
    `(helm-candidate-number                     ((t ( :foreground ,gruvbox-neutral_green ))))
    `(helm-ff-directory                         ((t ( :foreground ,gruvbox-neutral_purple ))))
    `(helm-ff-executable                        ((t ( :foreground ,gruvbox-turquoise4  ))))
    `(helm-ff-file                              ((t ( :foreground ,gruvbox-sienna ))))
    `(helm-ff-invalid-symlink                   ((t ( :foreground ,gruvbox-white         :background ,gruvbox-bright_red   ))))
    `(helm-ff-prefix                            ((t ( :foreground ,gruvbox-black         :background ,gruvbox-neutral_yellow))))
    `(helm-ff-symlink                           ((t ( :foreground ,gruvbox-neutral_orange ))))
    `(helm-grep-cmd-line                        ((t ( :foreground ,gruvbox-neutral_green ))))
    `(helm-grep-file                            ((t ( :foreground ,gruvbox-faded_purple ))))
    `(helm-grep-finish                          ((t ( :foreground ,gruvbox-turquoise4 ))))
    `(helm-grep-lineno                          ((t ( :foreground ,gruvbox-neutral_orange ))))
    `(helm-grep-match                           ((t ( :foreground ,gruvbox-neutral_yellow ))))
    `(helm-grep-running                         ((t ( :foreground ,gruvbox-neutral_red ))))
    `(helm-header                               ((t ( :foreground ,gruvbox-aquamarine4 ))))
    `(helm-helper                               ((t ( :foreground ,gruvbox-aquamarine4 ))))
    `(helm-history-deleted                      ((t ( :foreground ,gruvbox-black         :background ,gruvbox-bright_red   ))))
    `(helm-history-remote                       ((t ( :foreground ,gruvbox-faded_red ))))
    `(helm-lisp-completion-info                 ((t ( :foreground ,gruvbox-faded_orange ))))
    `(helm-lisp-show-completion                 ((t ( :foreground ,gruvbox-bright_red ))))
    `(helm-locate-finish                        ((t ( :foreground ,gruvbox-white         :background ,gruvbox-aquamarine4  ))))
    `(helm-match                                ((t ( :foreground ,gruvbox-neutral_orange ))))
    `(helm-moccur-buffer                        ((t ( :foreground ,gruvbox-bright_aqua :underline t                          ))))
    `(helm-prefarg                              ((t ( :foreground ,gruvbox-turquoise4 ))))
    `(helm-selection                            ((t ( :foreground ,gruvbox-white         :background ,gruvbox-dark2        ))))
    `(helm-selection-line                       ((t ( :foreground ,gruvbox-white         :background ,gruvbox-dark2        ))))
    `(helm-separator                            ((t ( :foreground ,gruvbox-faded_red ))))
    `(helm-source-header                        ((t ( :foreground ,gruvbox-light2 ))))
    `(helm-visible-mark                         ((t ( :foreground ,gruvbox-black         :background ,gruvbox-light3       ))))

    ;; company-mode
    `(company-scrollbar-bg              ((t (:background ,gruvbox-dark1))))
    `(company-scrollbar-fg              ((t (:background ,gruvbox-dark0_soft))))
    `(company-tooltip                   ((t (:background ,gruvbox-dark0_soft))))
    `(company-tooltip-annotation        ((t (:foreground ,gruvbox-neutral_green))))
    `(company-tooltip-selection         ((t (:foreground ,gruvbox-neutral_purple))))
    `(company-tooltip-common            ((t (:foreground ,gruvbox-neutral_blue :underline t))))
    `(company-tooltip-common-selection  ((t (:foreground ,gruvbox-neutral_blue :underline t))))
    `(company-preview-common            ((t (:foreground ,gruvbox-neutral_purple))))

    ;; Term
    `(term-color-black                  ((t (:foreground ,gruvbox-dark1))))
    `(term-color-blue                   ((t (:foreground ,gruvbox-neutral_blue))))
    `(term-color-cyan                   ((t (:foreground ,gruvbox-neutral_aqua))))
    `(term-color-green                  ((t (:foreground ,gruvbox-neutral_green))))
    `(term-color-magenta                ((t (:foreground ,gruvbox-neutral_purple))))
    `(term-color-red                    ((t (:foreground ,gruvbox-neutral_red))))
    `(term-color-white                  ((t (:foreground ,gruvbox-light1))))
    `(term-color-yellow                 ((t (:foreground ,gruvbox-neutral_yellow))))
    `(term-default-fg-color             ((t (:foreground ,gruvbox-light0))))
    `(term-default-bg-color             ((t (:background ,gruvbox-bg))))

    ;; message-mode
    `(message-header-to                 ((t (:inherit font-lock-variable-name-face))))
    `(message-header-cc                 ((t (:inherit font-lock-variable-name-face))))
    `(message-header-subject            ((t (:foreground ,gruvbox-neutral_orange :weight bold))))
    `(message-header-newsgroups         ((t (:foreground ,gruvbox-neutral_yellow :weight bold))))
    `(message-header-other              ((t (:inherit font-lock-variable-name-face))))
    `(message-header-name               ((t (:inherit font-lock-keyword-face))))
    `(message-header-xheader            ((t (:foreground ,gruvbox-faded_blue))))
    `(message-separator                 ((t (:inherit font-lock-comment-face))))
    `(message-cited-text                ((t (:inherit font-lock-comment-face))))
    `(message-mml                       ((t (:foreground ,gruvbox-faded_green :weight bold))))

    ;; org-mode
    `(org-hide                          ((t (:foreground ,gruvbox-dark0))))
    `(org-level-1                       ((t (:foreground ,gruvbox-neutral_blue))))
    `(org-level-2                       ((t (:foreground ,gruvbox-neutral_yellow))))
    `(org-level-3                       ((t (:foreground ,gruvbox-neutral_purple))))
    `(org-level-4                       ((t (:foreground ,gruvbox-neutral_red))))
    `(org-level-5                       ((t (:foreground ,gruvbox-neutral_green))))
    `(org-level-6                       ((t (:foreground ,gruvbox-neutral_aqua))))
    `(org-level-7                       ((t (:foreground ,gruvbox-faded_blue))))
    `(org-level-8                       ((t (:foreground ,gruvbox-neutral_orange))))
    `(org-special-keyword               ((t (:inherit font-lock-comment-face))))
    `(org-drawer                        ((t (:inherit font-lock-function-face))))
    `(org-column                        ((t (:background ,gruvbox-dark0))))
    `(org-column-title                  ((t (:background ,gruvbox-dark0 :underline t :weight bold))))
    `(org-warning                       ((t (:bold t :foreground ,gruvbox-neutral_red :weight bold :underline nil))))
    `(org-archived                      ((t (:foreground ,gruvbox-light0 :weight bold))))
    `(org-link                          ((t (:foreground ,gruvbox-faded_aqua :underline t))))
    `(org-footnote                      ((t (:foreground ,gruvbox-neutral_aqua :underline t))))
    `(org-ellipsis                      ((t (:foreground ,gruvbox-light4 :underline t))))
    `(org-date                          ((t (:foreground ,gruvbox-neutral_blue :underline t))))
    `(org-sexp-date                     ((t (:foreground ,gruvbox-faded_blue :underline t))))
    `(org-tag                           ((t (:bold t :weight bold))))
    `(org-list-dt                       ((t (:bold t :weight bold))))
    `(org-todo                          ((t (:bold t :foreground ,gruvbox-neutral_red :weight bold))))
    `(org-done                          ((t (:bold t :foreground ,gruvbox-neutral_aqua :weight bold))))
    `(org-agenda-done                   ((t (:foreground ,gruvbox-neutral_aqua))))
    `(org-headline-done                 ((t (:foreground ,gruvbox-neutral_aqua))))
    `(org-table                         ((t (:foreground ,gruvbox-neutral_blue))))
    `(org-formula                       ((t (:foreground ,gruvbox-neutral_yellow))))
    `(org-document-title                ((t (:foreground ,gruvbox-faded_blue))))
    `(org-document-info                 ((t (:foreground ,gruvbox-faded_blue))))
    `(org-agenda-structure              ((t (:inherit font-lock-comment-face))))
    `(org-agenda-date-today             ((t (:foreground ,gruvbox-light0 :weight bold :italic t))))
    `(org-scheduled                     ((t (:foreground ,gruvbox-neutral_yellow))))
    `(org-scheduled-today               ((t (:foreground ,gruvbox-neutral_blue))))
    `(org-scheduled-previously          ((t (:foreground ,gruvbox-faded_red))))
    `(org-upcoming-deadline             ((t (:inherit font-lock-keyword-face))))
    `(org-deadline-announce             ((t (:foreground ,gruvbox-faded_red))))
    `(org-time-grid                     ((t (:foreground ,gruvbox-faded_orange))))

    ;; org-habit
    `(org-habit-clear-face              ((t (:background ,gruvbox-faded_blue))))
    `(org-habit-clear-future-face       ((t (:background ,gruvbox-neutral_blue))))
    `(org-habit-ready-face              ((t (:background ,gruvbox-faded_green))))
    `(org-habit-ready-future-face       ((t (:background ,gruvbox-neutral_green))))
    `(org-habit-alert-face              ((t (:background ,gruvbox-faded_yellow))))
    `(org-habit-alert-future-face       ((t (:background ,gruvbox-neutral_yellow))))
    `(org-habit-overdue-face            ((t (:background ,gruvbox-faded_red))))
    `(org-habit-overdue-future-face     ((t (:background ,gruvbox-neutral_red))))

    ;; elfeed
    `(elfeed-search-title-face          ((t (:foreground ,gruvbox-medium))))
    `(elfeed-search-unread-title-face   ((t (:foreground ,gruvbox-light0))))
    `(elfeed-search-date-face           ((t (:inherit font-lock-builtin-face :underline t))))
    `(elfeed-search-feed-face           ((t (:inherit font-lock-variable-name-face))))
    `(elfeed-search-tag-face            ((t (:inherit font-lock-keyword-face))))
    `(elfeed-search-last-update-face    ((t (:inherit font-lock-comment-face))))
    `(elfeed-search-unread-count-face   ((t (:inherit font-lock-comment-face))))
    `(elfeed-search-filter-face         ((t (:inherit font-lock-string-face))))

    ;; Smart-mode-line
    `(sml/global            ((t (:foreground ,gruvbox-burlywood4 :inverse-video nil))))
    `(sml/modes             ((t (:foreground ,gruvbox-bright_green))))
    `(sml/filename          ((t (:foreground ,gruvbox-bright_red :weight bold))))
    `(sml/prefix            ((t (:foreground ,gruvbox-light1))))
    `(sml/read-only         ((t (:foreground ,gruvbox-neutral_blue))))
    `(persp-selected-face   ((t (:foreground ,gruvbox-neutral_orange))))

        ;;isearch
    `(isearch                       ((t (:foreground ,gruvbox-black :background ,gruvbox-neutral_orange))))
    `(lazy-highlight           ((t (:foreground ,gruvbox-black :background ,gruvbox-neutral_yellow))))
    `(isearch-fail                  ((t (:foreground ,gruvbox-light0 :background ,gruvbox-bright_red))))

    ;; markdown-mode
    `(markdown-header-face-1                       ((t (:foreground ,gruvbox-neutral_blue))))
    `(markdown-header-face-2                       ((t (:foreground ,gruvbox-neutral_yellow))))
    `(markdown-header-face-3                       ((t (:foreground ,gruvbox-neutral_purple))))
    `(markdown-header-face-4                       ((t (:foreground ,gruvbox-neutral_red))))
    `(markdown-header-face-5                       ((t (:foreground ,gruvbox-neutral_green))))
    `(markdown-header-face-6                       ((t (:foreground ,gruvbox-neutral_aqua))))

    ;; anzu-mode
    `(anzu-mode-line        ((t (:foreground ,gruvbox-bright_yellow :weight bold))))
    `(anzu-match-1          ((t (:background ,gruvbox-bright_green))))
    `(anzu-match-2          ((t (:background ,gruvbox-faded_yellow))))
    `(anzu-match-3          ((t (:background ,gruvbox-aquamarine4))))
    `(anzu-replace-to       ((t (:foreground ,gruvbox-bright_yellow))))
    `(anzu-replace-highlight ((t (:inherit isearch))))

    ;; Ace-jump-mode
    `(ace-jump-face-background  ((t (:foreground ,gruvbox-light4 :background ,gruvbox-bg :inverse-video nil))))
    `(ace-jump-face-foreground  ((t (:foreground ,gruvbox-bright_red :background ,gruvbox-bg :inverse-video nil :box 1))))

    ;; Ace-window
    `(aw-background-face        ((t (:forground  ,gruvbox-light1 :background ,gruvbox-bg :inverse-video nil))))
    `(aw-leading-char-face ((t (:foreground ,gruvbox-bright_orange :background ,gruvbox-bg :height 4.0 :box (:line-width 1 :color ,gruvbox-bright_orange)))))

    ;; show-paren
    `(show-paren-match      ((t (:background ,gruvbox-dark3 :weight bold))))
    `(show-paren-mismatch   ((t (:background ,gruvbox-bright_red :foreground ,gruvbox-dark3 :weight bold))))

    ;; MODE SUPPORT: dired+
    `(diredp-file-name                          ((t (:foreground ,gruvbox-light2 ))))
    `(diredp-file-suffix                        ((t (:foreground ,gruvbox-light4 ))))
    `(diredp-compressed-file-suffix             ((t (:foreground ,gruvbox-faded_blue ))))
    `(diredp-dir-name                           ((t (:foreground ,gruvbox-faded_blue ))))
    `(diredp-dir-heading                        ((t (:foreground ,gruvbox-bright_blue ))))
    `(diredp-symlink                            ((t (:foreground ,gruvbox-bright_orange ))))
    `(diredp-date-time                          ((t (:foreground ,gruvbox-light3 ))))
    `(diredp-number                             ((t (:foreground ,gruvbox-faded_blue ))))
    `(diredp-no-priv                            ((t (:foreground ,gruvbox-dark4 ))))
    `(diredp-other-priv                         ((t (:foreground ,gruvbox-dark2 ))))
    `(diredp-rare-priv                          ((t (:foreground ,gruvbox-dark4 ))))
    `(diredp-ignored-file-name                  ((t (:foreground ,gruvbox-dark4 ))))

    `(diredp-dir-priv                           ((t (:foreground ,gruvbox-faded_blue  :background ,gruvbox-dark_blue))))
    `(diredp-exec-priv                          ((t (:foreground ,gruvbox-faded_blue  :background ,gruvbox-dark_blue))))
    `(diredp-link-priv                          ((t (:foreground ,gruvbox-faded_aqua  :background ,gruvbox-dark_aqua))))
    `(diredp-read-priv                          ((t (:foreground ,gruvbox-bright_red  :background ,gruvbox-dark_red))))
    `(diredp-write-priv                         ((t (:foreground ,gruvbox-bright_aqua :background ,gruvbox-dark_aqua))))

)


(custom-theme-set-variables
  'gruvbox

  `(ansi-color-names-vector [,gruvbox-dark1 ,gruvbox-neutral_red
    ,gruvbox-neutral_green ,gruvbox-neutral_yellow ,gruvbox-neutral_blue
    ,gruvbox-neutral_purple ,gruvbox-neutral_aqua ,gruvbox-light1])))

(defun gruvbox-set-ansi-color-names-vector ()
  "Give comint and the like the same colours as the term colours we set."
  (setq ansi-color-names-vector
    [term-color-black term-color-red term-color-green term-color-yellow term-color-blue
     term-color-purple term-color-aqua term-color-white]))

;;;###autoload
(and load-file-name
    (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path
                 (file-name-as-directory
                  (file-name-directory load-file-name))))

(provide-theme 'gruvbox)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; gruvbox-theme.el ends here
