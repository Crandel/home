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

(defcustom gruvbox-contrast 'hard
  "Contrast level for the theme background."
  :options '(soft medium hard))

(deftheme gruvbox "A retro-groove colour theme")
(let* ((gruvbox-dark0_hard  "#1d2021")
      (gruvbox-dark0        "#282828")
      (gruvbox-dark0_soft   "#32302f")
      (gruvbox-dark1        "#3c3836")
      (gruvbox-dark2        "#504945")
      (gruvbox-dark3        "#665c54")
      (gruvbox-dark4        "#7c6f64")

      (gruvbox-gray         "#928374")
      (gruvbox-medium       "#928374")

      (gruvbox-light0_hard  "#f9f5d7")
      (gruvbox-light0       "#fbf1c7")
      (gruvbox-light0_soft  "#f2e5bc")
      (gruvbox-light1       "#ebdbb2")
      (gruvbox-light2       "#d5c4a1")
      (gruvbox-light3       "#bdae93")
      (gruvbox-light4       "#a89984")

      (gruvbox-bright_red      "#fb4934")
      (gruvbox-bright_green    "#b8bb26")
      (gruvbox-bright_yellow   "#fabd2f")
      (gruvbox-bright_blue     "#83a598")
      (gruvbox-bright_purple   "#d3869b")
      (gruvbox-bright_aqua     "#8ec07c")
      (gruvbox-bright_orange   "#fe8019")

      ;; neutral, no 256-color code, requested, nice work-around meanwhile
      (gruvbox-neutral_red     "#cc241d")
      (gruvbox-neutral_green   "#98971a")
      (gruvbox-neutral_yellow  "#d79921")
      (gruvbox-neutral_blue    "#458588")
      (gruvbox-neutral_purple  "#b16286")
      (gruvbox-neutral_aqua    "#689d6a")
      (gruvbox-neutral_orange  "#d65d0e")

      (gruvbox-faded_red       "#9d0006")
      (gruvbox-faded_green     "#79740e")
      (gruvbox-faded_yellow    "#b57614")
      (gruvbox-faded_blue      "#076678")
      (gruvbox-faded_purple    "#8f3f71")
      (gruvbox-faded_aqua      "#427b58")
      (gruvbox-faded_orange    "#af3a03")

      (gruvbox-dark_red         "#421E1E")
      (gruvbox-dark_green       "#071907")
      (gruvbox-dark_yellow      "#181907")
      (gruvbox-dark_blue        "#2B3C44")
      (gruvbox-dark_purple      "#220A29")
      (gruvbox-dark_aqua        "#36473A")

      (gruvbox-delimiter-one     "#458588")
      (gruvbox-delimiter-two     "#b16286")
      (gruvbox-delimiter-three   "#8ec07c")
      (gruvbox-delimiter-four    "#d65d0e")
      (gruvbox-white             "#FFFFFF")
      (gruvbox-black             "#000000")
      (gruvbox-sienna            "#DD6F48")
      (gruvbox-darkslategray4    "#528B8B")
      (gruvbox-lightblue4        "#66999D")
      (gruvbox-burlywood4        "#BBAA97")
      (gruvbox-aquamarine4       "#83A598")
      (gruvbox-turquoise4        "#61ACBB")
      (gruvbox-ediff-current-diff-A         "#4f2121")
      (gruvbox-ediff-current-diff-B         "#243c24")
      (gruvbox-ediff-current-diff-C         "#4f214f")
      (gruvbox-ediff-current-diff-Ancestor  "#21214f")
      (gruvbox-ediff-fine-diff-A            "#761919")
      (gruvbox-ediff-fine-diff-B            "#1c691c")
      (gruvbox-ediff-fine-diff-C            "#761976")
      (gruvbox-ediff-fine-diff-Ancestor     "#12129d")

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
    `(mode-line                         ((t (:box unspecified :background ,gruvbox-dark2 :foreground ,gruvbox-light2))))
    `(mode-line-inactive                ((t (:box unspecified :background ,gruvbox-dark1 :foreground ,gruvbox-light4))))
    `(fringe                            ((t (:background ,gruvbox-bg))))
    `(linum                             ((t (:background ,gruvbox-bg :foreground ,gruvbox-dark4))))
    `(hl-line                           ((t (:background ,gruvbox-dark1))))
    `(region                            ((t (:background ,gruvbox-dark2)))) ;;selection
    `(secondary-selection               ((t (:background ,gruvbox-dark1))))
    `(minibuffer-prompt                 ((t (:background ,gruvbox-bg :foreground ,gruvbox-neutral_green :bold t))))
    `(vertical-border                   ((t (:foreground ,gruvbox-dark2))))
    `(link                              ((t (:foreground ,gruvbox-faded_blue :underline t))))
    `(highlight                         ((t (:box t :underline t))))
    `(shadow                            ((t (:foreground ,gruvbox-dark4))))

    ;; Built-in syntax
    `(font-lock-builtin-face            ((t (:foreground ,gruvbox-neutral_yellow))))
    `(font-lock-constant-face           ((t (:foreground ,gruvbox-turquoise4))))
    `(font-lock-comment-face            ((t (:foreground ,gruvbox-dark4))))
    `(font-lock-function-name-face      ((t (:foreground ,gruvbox-neutral_orange))))
    `(font-lock-keyword-face            ((t (:foreground ,gruvbox-neutral_green))))
    `(font-lock-string-face             ((t (:foreground ,gruvbox-neutral_aqua))))
    `(font-lock-variable-name-face      ((t (:foreground ,gruvbox-aquamarine4))))
    `(font-lock-type-face               ((t (:foreground ,gruvbox-faded_aqua))))
    `(font-lock-warning-face            ((t (:foreground ,gruvbox-neutral_red :bold t))))

    ;; Diffs
    `(diff-header                       ((t (:background ,gruvbox-dark1))))
    `(diff-file-header                  ((t (:background ,gruvbox-dark2))))
    `(diff-hunk-header                  ((t (:background ,gruvbox-dark2))))
    `(diff-context                      ((t (:background ,gruvbox-dark1 :foreground ,gruvbox-light1))))
    `(diff-changed                      ((t (:background unspecified :foreground ,gruvbox-light1))))
    `(diff-added                        ((t (:background unspecified :foreground ,gruvbox-neutral_green))))
    `(diff-refine-added                 ((t (:background unspecified :foreground ,gruvbox-bright_green))))
    `(diff-removed                      ((t (:background unspecified :foreground ,gruvbox-neutral_red))))
    `(diff-refine-removed               ((t (:background unspecified :foreground ,gruvbox-bright_red))))
    `(diff-indicator-changed            ((t (:inherit 'diff-changed))))
    `(diff-indicator-added              ((t (:inherit 'diff-added))))
    `(diff-indicator-removed            ((t (:inherit 'diff-removed))))

    ;; Ediff
    `(ediff-even-diff-A                 ((t (:background ,gruvbox-dark1))))
    `(ediff-even-diff-B                 ((t (:background ,gruvbox-dark1))))
    `(ediff-even-diff-C                 ((t (:background ,gruvbox-dark1))))
    `(ediff-even-diff-Ancestor          ((t (:background ,gruvbox-dark1))))
    `(ediff-odd-diff-A                  ((t (:background ,gruvbox-dark2))))
    `(ediff-odd-diff-B                  ((t (:background ,gruvbox-dark2))))
    `(ediff-odd-diff-C                  ((t (:background ,gruvbox-dark2))))
    `(ediff-odd-diff-Ancestor           ((t (:background ,gruvbox-dark2))))
    `(ediff-fine-diff-A                 ((t (:background ,gruvbox-ediff-fine-diff-A))))
    `(ediff-fine-diff-Ancestor          ((t (:background ,gruvbox-ediff-fine-diff-Ancestor))))
    `(ediff-fine-diff-B                 ((t (:background ,gruvbox-ediff-fine-diff-B))))
    `(ediff-fine-diff-C                 ((t (:background ,gruvbox-ediff-fine-diff-C))))
    `(ediff-current-diff-A              ((t (:background ,gruvbox-ediff-current-diff-A))))
    `(ediff-current-diff-Ancestor       ((t (:background ,gruvbox-ediff-current-diff-Ancestor))))
    `(ediff-current-diff-B              ((t (:background ,gruvbox-ediff-current-diff-B))))
    `(ediff-current-diff-C              ((t (:background ,gruvbox-ediff-current-diff-C))))

    ;; Highlight indentation mode
    `(highlight-indentation-current-column-face ((t (:background ,gruvbox-dark2 ))))
    `(highlight-indentation-face                ((t (:background ,gruvbox-dark0_hard ))))

    ;;isearch
    `(isearch                           ((t (:foreground ,gruvbox-black :background ,gruvbox-neutral_orange))))
    `(lazy-highlight                    ((t (:foreground ,gruvbox-black :background ,gruvbox-neutral_yellow))))
    `(isearch-fail                      ((t (:foreground ,gruvbox-light0 :background ,gruvbox-bright_red))))

    ;; linum-relative
    `(linum-relative-current-face       ((t (:background ,gruvbox-dark1 :foreground ,gruvbox-light4))))

    ;; show-paren
    `(show-paren-match                  ((t (:background ,gruvbox-dark3 :weight bold))))
    `(show-paren-mismatch               ((t (:background ,gruvbox-bright_red :foreground ,gruvbox-dark3 :weight bold))))

    ;; tool tips
    `(tooltip                           ((t (:foreground ,gruvbox-light1 :background ,gruvbox-dark1))))

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

    ;; whitespace-mode
    `(whitespace-space                  ((t (:background ,gruvbox-bg :foreground ,gruvbox-dark4))))
    `(whitespace-hspace                 ((t (:background ,gruvbox-bg :foreground ,gruvbox-dark4))))
    `(whitespace-tab                    ((t (:background ,gruvbox-bg :foreground ,gruvbox-dark4))))
    `(whitespace-newline                ((t (:background ,gruvbox-bg :foreground ,gruvbox-dark4))))
    `(whitespace-trailing               ((t (:background ,gruvbox-dark1 :foreground ,gruvbox-neutral_red))))
    `(whitespace-line                   ((t (:background ,gruvbox-dark1))))
    `(whitespace-space-before-tab       ((t (:background ,gruvbox-bg :foreground ,gruvbox-dark4))))
    `(whitespace-indentation            ((t (:background ,gruvbox-bg :foreground ,gruvbox-dark4))))
    `(whitespace-empty                  ((t (:background unspecified :foreground unspecified))))
    `(whitespace-space-after-tab        ((t (:background ,gruvbox-bg :foreground ,gruvbox-dark4))))

    ;; 3-rd party packages

    ;; ag (The Silver Searcher)
    `(ag-hit-face                       ((t (:foreground ,gruvbox-neutral_blue))))
    `(ag-match-face                     ((t (:foreground ,gruvbox-neutral_green))))

    ;; avy
    `(avy-background-face ((t (:foreground ,gruvbox-neutral_green))))
    `(avy-lead-face   ((t (:background ,gruvbox-dark_aqua :foreground ,gruvbox-neutral_green :weight bold))))
    `(avy-lead-face-0 ((t (:inherit 'avy-lead-face :background ,gruvbox-dark_blue))))
    `(avy-lead-face-1 ((t (:inherit 'avy-lead-face :background ,gruvbox-dark_purple))))
    `(avy-lead-face-2 ((t (:inherit 'avy-lead-face :background ,gruvbox-dark_yellow))))

    ;; company-mode
    `(company-scrollbar-bg                 ((t (:background ,gruvbox-dark1))))
    `(company-scrollbar-fg                 ((t (:background ,gruvbox-dark0_soft))))
    `(company-tooltip                      ((t (:background ,gruvbox-dark0_soft))))
    `(company-tooltip-annotation           ((t (:foreground ,gruvbox-neutral_green))))
    `(company-tooltip-annotation-selection ((t (:inherit 'company-tooltip-annotation))))
    `(company-tooltip-selection            ((t (:foreground ,gruvbox-neutral_purple))))
    `(company-tooltip-common               ((t (:foreground ,gruvbox-neutral_blue :underline t))))
    `(company-tooltip-common-selection     ((t (:foreground ,gruvbox-neutral_blue :underline t))))
    `(company-preview                      ((t (:background ,gruvbox-lightblue4))))
    `(company-preview-common               ((t (:foreground ,gruvbox-neutral_purple))))
    `(company-preview-search               ((t (:background ,gruvbox-turquoise4))))
    `(company-template-field               ((t (:foreground ,gruvbox-black :background ,gruvbox-bright_yellow))))
    `(company-echo-common                  ((t (:foreground ,gruvbox-faded_red))))

    ;; corfu
    `(corfu-background                  ((t (:background ,gruvbox-dark1))))
    `(corfu-current                     ((t (:foreground ,gruvbox-bright_purple :background ,gruvbox-dark2))))
    `(corfu-bar                         ((t (:background ,gruvbox-dark2))))
    `(corfu-border                      ((t (:background ,gruvbox-dark1))))

    ;; MODE SUPPORT: dired+
    `(diredp-file-name                  ((t (:foreground ,gruvbox-light2 ))))
    `(diredp-file-suffix                ((t (:foreground ,gruvbox-light4 ))))
    `(diredp-compressed-file-suffix     ((t (:foreground ,gruvbox-faded_blue ))))
    `(diredp-dir-name                   ((t (:foreground ,gruvbox-faded_blue ))))
    `(diredp-dir-heading                ((t (:foreground ,gruvbox-bright_blue ))))
    `(diredp-symlink                    ((t (:foreground ,gruvbox-bright_orange ))))
    `(diredp-date-time                  ((t (:foreground ,gruvbox-light3 ))))
    `(diredp-number                     ((t (:foreground ,gruvbox-faded_blue ))))
    `(diredp-no-priv                    ((t (:foreground ,gruvbox-dark4 ))))
    `(diredp-other-priv                 ((t (:foreground ,gruvbox-dark2 ))))
    `(diredp-rare-priv                  ((t (:foreground ,gruvbox-dark4 ))))
    `(diredp-ignored-file-name          ((t (:foreground ,gruvbox-dark4 ))))
    `(diredp-dir-priv                   ((t (:foreground ,gruvbox-faded_blue  :background ,gruvbox-dark_blue))))
    `(diredp-exec-priv                  ((t (:foreground ,gruvbox-faded_blue  :background ,gruvbox-dark_blue))))
    `(diredp-link-priv                  ((t (:foreground ,gruvbox-faded_aqua  :background ,gruvbox-dark_aqua))))
    `(diredp-read-priv                  ((t (:foreground ,gruvbox-bright_red  :background ,gruvbox-dark_red))))
    `(diredp-write-priv                 ((t (:foreground ,gruvbox-bright_aqua :background ,gruvbox-dark_aqua))))

    ;; elfeed
    `(elfeed-search-title-face          ((t (:foreground ,gruvbox-medium))))
    `(elfeed-search-unread-title-face   ((t (:foreground ,gruvbox-light0))))
    `(elfeed-search-date-face           ((t (:inherit font-lock-builtin-face :underline t))))
    `(elfeed-search-feed-face           ((t (:inherit font-lock-variable-name-face))))
    `(elfeed-search-tag-face            ((t (:inherit font-lock-keyword-face))))
    `(elfeed-search-last-update-face    ((t (:inherit font-lock-comment-face))))
    `(elfeed-search-unread-count-face   ((t (:inherit font-lock-comment-face))))
    `(elfeed-search-filter-face         ((t (:inherit font-lock-string-face))))

    ;; elscreen
    `(elscreen-tab-background-face      ((t (:box unspecified :background ,gruvbox-bg)))) ;; Tab bar, not the tabs
    `(elscreen-tab-control-face         ((t (:box unspecified :background ,gruvbox-dark2 :foreground ,gruvbox-neutral_red :underline unspecified)))) ;; The controls
    `(elscreen-tab-current-screen-face  ((t (:box unspecified :background ,gruvbox-dark4 :foreground ,gruvbox-dark0)))) ;; Current tab
    `(elscreen-tab-other-screen-face    ((t (:box unspecified :background ,gruvbox-dark2 :foreground ,gruvbox-light4 :underline unspecified)))) ;; Inactive tab

    ; flycheck
    `(flycheck-warning                  ((t (:underline (:style wave :color ,gruvbox-bright_yellow)))))
    `(flycheck-error                    ((t (:underline (:style wave :color ,gruvbox-bright_red)))))
    `(flycheck-info                     ((t (:underline (:style wave :color ,gruvbox-bright_blue)))))
    `(flycheck-fringe-warning           ((t (:foreground ,gruvbox-bright_yellow))))
    `(flycheck-fringe-error             ((t (:foreground ,gruvbox-bright_red))))
    `(flycheck-fringe-info              ((t (:foreground ,gruvbox-bright_blue))))
    `(flycheck-error-list-warning       ((t (:foreground ,gruvbox-bright_yellow :bold t))))
    `(flycheck-error-list-error         ((t (:foreground ,gruvbox-bright_red :bold t))))
    `(flycheck-error-list-info          ((t (:foreground ,gruvbox-bright_blue :bold t))))

    ;; git-gutter
    `(git-gutter:modified               ((t (:background ,gruvbox-faded_blue :foreground ,gruvbox-faded_blue))))
    `(git-gutter:added                  ((t (:background ,gruvbox-faded_green :foreground ,gruvbox-faded_green))))
    `(git-gutter:deleted                ((t (:background ,gruvbox-faded_red :foreground ,gruvbox-faded_red))))

    ;; js2-mode
    `(js2-warning                       ((t (:underline (:color ,gruvbox-bright_yellow :style wave)))))
    `(js2-error                         ((t (:underline (:color ,gruvbox-bright_red :style wave)))))
    `(js2-external-variable             ((t (:underline (:color ,gruvbox-bright_aqua :style wave)))))
    `(js2-jsdoc-tag                     ((t (:background unspecified :foreground ,gruvbox-medium ))))
    `(js2-jsdoc-type                    ((t (:background unspecified :foreground ,gruvbox-light4 ))))
    `(js2-jsdoc-value                   ((t (:background unspecified :foreground ,gruvbox-light3 ))))
    `(js2-function-param                ((t (:background unspecified :foreground ,gruvbox-bright_aqua ))))
    `(js2-function-call                 ((t (:background unspecified :foreground ,gruvbox-bright_blue ))))
    `(js2-instance-member               ((t (:background unspecified :foreground ,gruvbox-bright_orange ))))
    `(js2-private-member                ((t (:background unspecified :foreground ,gruvbox-faded_yellow ))))
    `(js2-private-function-call         ((t (:background unspecified :foreground ,gruvbox-faded_aqua ))))
    `(js2-jsdoc-html-tag-name           ((t (:background unspecified :foreground ,gruvbox-light4 ))))
    `(js2-jsdoc-html-tag-delimiter      ((t (:background unspecified :foreground ,gruvbox-light3 ))))

    ;; Helm
    ;; `(helm-M-x-key                              ((t ( :foreground ,gruvbox-neutral_orange  ))))
    ;; `(helm-action                               ((t ( :foreground ,gruvbox-white :underline t ))))
    ;; `(helm-bookmark-addressbook                 ((t ( :foreground ,gruvbox-neutral_red ))))
    ;; `(helm-bookmark-directory                   ((t ( :foreground ,gruvbox-bright_purple ))))
    ;; `(helm-bookmark-file                        ((t ( :foreground ,gruvbox-faded_blue ))))
    ;; `(helm-bookmark-gnus                        ((t ( :foreground ,gruvbox-faded_purple ))))
    ;; `(helm-bookmark-info                        ((t ( :foreground ,gruvbox-turquoise4 ))))
    ;; `(helm-bookmark-man                         ((t ( :foreground ,gruvbox-sienna ))))
    ;; `(helm-bookmark-w3m                         ((t ( :foreground ,gruvbox-neutral_yellow ))))
    ;; `(helm-buffer-directory                     ((t ( :foreground ,gruvbox-white         :background ,gruvbox-bright_blue  ))))
    ;; `(helm-buffer-not-saved                     ((t ( :foreground ,gruvbox-faded_red ))))
    ;; `(helm-buffer-process                       ((t ( :foreground ,gruvbox-burlywood4 ))))
    ;; `(helm-buffer-saved-out                     ((t ( :foreground ,gruvbox-bright_red ))))
    ;; `(helm-buffer-size                          ((t ( :foreground ,gruvbox-bright_purple ))))
    ;; `(helm-candidate-number                     ((t ( :foreground ,gruvbox-neutral_green ))))
    ;; `(helm-ff-directory                         ((t ( :foreground ,gruvbox-neutral_purple ))))
    ;; `(helm-ff-executable                        ((t ( :foreground ,gruvbox-turquoise4  ))))
    ;; `(helm-ff-file                              ((t ( :foreground ,gruvbox-sienna ))))
    ;; `(helm-ff-invalid-symlink                   ((t ( :foreground ,gruvbox-white         :background ,gruvbox-bright_red   ))))
    ;; `(helm-ff-prefix                            ((t ( :foreground ,gruvbox-black         :background ,gruvbox-neutral_yellow))))
    ;; `(helm-ff-symlink                           ((t ( :foreground ,gruvbox-neutral_orange ))))
    ;; `(helm-grep-cmd-line                        ((t ( :foreground ,gruvbox-neutral_green ))))
    ;; `(helm-grep-file                            ((t ( :foreground ,gruvbox-faded_purple ))))
    ;; `(helm-grep-finish                          ((t ( :foreground ,gruvbox-turquoise4 ))))
    ;; `(helm-grep-lineno                          ((t ( :foreground ,gruvbox-neutral_orange ))))
    ;; `(helm-grep-match                           ((t ( :foreground ,gruvbox-neutral_yellow ))))
    ;; `(helm-grep-running                         ((t ( :foreground ,gruvbox-neutral_red ))))
    ;; `(helm-header                               ((t ( :foreground ,gruvbox-aquamarine4 ))))
    ;; `(helm-helper                               ((t ( :foreground ,gruvbox-aquamarine4 ))))
    ;; `(helm-history-deleted                      ((t ( :foreground ,gruvbox-black         :background ,gruvbox-bright_red   ))))
    ;; `(helm-history-remote                       ((t ( :foreground ,gruvbox-faded_red ))))
    ;; `(helm-lisp-completion-info                 ((t ( :foreground ,gruvbox-faded_orange ))))
    ;; `(helm-lisp-show-completion                 ((t ( :foreground ,gruvbox-bright_red ))))
    ;; `(helm-locate-finish                        ((t ( :foreground ,gruvbox-white         :background ,gruvbox-aquamarine4  ))))
    ;; `(helm-match                                ((t ( :foreground ,gruvbox-neutral_orange ))))
    ;; `(helm-moccur-buffer                        ((t ( :foreground ,gruvbox-bright_aqua :underline t                          ))))
    ;; `(helm-prefarg                              ((t ( :foreground ,gruvbox-turquoise4 ))))
    ;; `(helm-selection                            ((t ( :foreground ,gruvbox-white         :background ,gruvbox-dark2        ))))
    ;; `(helm-selection-line                       ((t ( :foreground ,gruvbox-white         :background ,gruvbox-dark2        ))))
    ;; `(helm-separator                            ((t ( :foreground ,gruvbox-faded_red ))))
    ;; `(helm-source-header                        ((t ( :foreground ,gruvbox-light2 ))))
    ;; `(helm-visible-mark                         ((t ( :foreground ,gruvbox-black         :background ,gruvbox-light3       ))))

    ;; hydra
    `(hydra-face-red                    ((t (:foreground ,gruvbox-bright_red :weight bold))))
    `(hydra-face-blue                   ((t (:foreground ,gruvbox-bright_blue :weight bold))))
    `(hydra-face-amaranth               ((t (:foreground ,gruvbox-bright_yellow :weight bold))))
    `(hydra-face-pink                   ((t (:foreground ,gruvbox-bright_purple :weight bold))))
    `(hydra-face-teal                   ((t (:foreground ,gruvbox-bright_aqua :weight bold))))

    ;; lsp
    `(lsp-lsp-flycheck-warning-unnecessary-face ((t (:underline (:color ,gruvbox-bright_orange :style wave)
                                                             :foreground ,gruvbox-burlywood4))))
    `(lsp-ui-doc-background                     ((t (:background ,gruvbox-dark3))))
    `(lsp-ui-doc-header                         ((t (:background ,gruvbox-faded_blue))))
    `(lsp-ui-peek-filename                      ((t (:foreground ,gruvbox-bright_red))))
    `(lsp-ui-sideline-code-action               ((t (:foreground ,gruvbox-bright_yellow))))
    `(lsp-ui-sideline-current-symbol            ((t (:foreground ,gruvbox-faded_aqua))))
    `(lsp-ui-sideline-symbol                    ((t (:foreground ,gruvbox-gray))))

    ;; magit
    `(magit-bisect-bad                  ((t (:foreground ,gruvbox-faded_red))))
    `(magit-bisect-good                 ((t (:foreground ,gruvbox-faded_green))))
    `(magit-bisect-skip                 ((t (:foreground ,gruvbox-faded_yellow))))
    `(magit-blame-heading               ((t (:foreground ,gruvbox-light0 :background ,gruvbox-dark2))))
    `(magit-branch-local                ((t (:foreground ,gruvbox-bright_blue))))
    `(magit-branch-current              ((t (:underline ,gruvbox-bright_blue :inherit 'magit-branch-local))))
    `(magit-branch-remote               ((t (:foreground ,gruvbox-bright_green))))
    `(magit-cherry-equivalent           ((t (:foreground ,gruvbox-bright_purple))))
    `(magit-cherry-unmatched            ((t (:foreground ,gruvbox-bright_aqua))))
    `(magit-diff-added                  ((t (:foreground ,gruvbox-bright_green))))
    `(magit-diff-added-highlight        ((t (:foreground ,gruvbox-bright_green :inherit 'magit-diff-context-highlight))))
    `(magit-diff-base                   ((t (:background ,gruvbox-faded_yellow :foreground ,gruvbox-light2))))
    `(magit-diff-base-highlight         ((t (:background ,gruvbox-faded_yellow :foreground ,gruvbox-light0))))
    `(magit-diff-context                ((t (:foreground ,gruvbox-dark1  :foreground ,gruvbox-light1))))
    `(magit-diff-context-highlight      ((t (:background ,gruvbox-dark1 :foreground ,gruvbox-light0))))
    `(magit-diff-hunk-heading           ((t (:background ,gruvbox-dark3 :foreground ,gruvbox-light2))))
    `(magit-diff-hunk-heading-highlight ((t (:background ,gruvbox-dark2 :foreground ,gruvbox-light0))))
    `(magit-diff-hunk-heading-selection ((t (:background ,gruvbox-dark2 :foreground ,gruvbox-bright_orange))))
    `(magit-diff-lines-heading          ((t (:background ,gruvbox-faded_orange :foreground ,gruvbox-light0))))
    `(magit-diff-removed                ((t (:foreground ,gruvbox-bright_red))))
    `(magit-diff-removed-highlight      ((t (:foreground ,gruvbox-bright_red :inherit 'magit-diff-context-highlight))))
    `(magit-diffstat-added              ((t (:foreground ,gruvbox-faded_green))))
    `(magit-diffstat-removed            ((t (:foreground ,gruvbox-faded_red))))
    `(magit-dimmed                      ((t (:foreground ,gruvbox-dark4))))
    `(magit-hash                        ((t (:foreground ,gruvbox-bright_blue))))
    `(magit-log-author                  ((t (:foreground ,gruvbox-bright_red))))
    `(magit-log-date                    ((t (:foreground ,gruvbox-bright_aqua))))
    `(magit-log-graph                   ((t (:foreground ,gruvbox-dark4))))
    `(magit-process-ng                  ((t (:foreground ,gruvbox-bright_red :weight bold))))
    `(magit-process-ok                  ((t (:foreground ,gruvbox-bright_green :weight bold))))
    `(magit-reflog-amend                ((t (:foreground ,gruvbox-bright_purple))))
    `(magit-reflog-checkout             ((t (:foreground ,gruvbox-bright_blue))))
    `(magit-reflog-cherry-pick          ((t (:foreground ,gruvbox-bright_green))))
    `(magit-reflog-commit               ((t (:foreground ,gruvbox-bright_green))))
    `(magit-reflog-merge                ((t (:foreground ,gruvbox-bright_green))))
    `(magit-reflog-other                ((t (:foreground ,gruvbox-bright_aqua))))
    `(magit-reflog-rebase               ((t (:foreground ,gruvbox-bright_purple))))
    `(magit-reflog-remote               ((t (:foreground ,gruvbox-bright_blue))))
    `(magit-reflog-reset                ((t (:foreground ,gruvbox-bright_red))))
    `(magit-refname                     ((t (:foreground ,gruvbox-light4))))
    `(magit-section-heading             ((t (:foreground ,gruvbox-bright_yellow :weight bold))))
    `(magit-section-heading-selection   ((t (:foreground ,gruvbox-faded_yellow))))
    `(magit-section-highlight           ((t (:background ,gruvbox-dark1))))
    `(magit-sequence-drop               ((t (:foreground ,gruvbox-faded_yellow))))
    `(magit-sequence-head               ((t (:foreground ,gruvbox-bright_aqua))))
    `(magit-sequence-part               ((t (:foreground ,gruvbox-bright_yellow))))
    `(magit-sequence-stop               ((t (:foreground ,gruvbox-bright_green))))
    `(magit-signature-bad               ((t (:foreground ,gruvbox-bright_red :weight bold))))
    `(magit-signature-error             ((t (:foreground ,gruvbox-bright_red))))
    `(magit-signature-expired           ((t (:foreground ,gruvbox-bright_orange))))
    `(magit-signature-good              ((t (:foreground ,gruvbox-bright_green))))
    `(magit-signature-revoked           ((t (:foreground ,gruvbox-bright_purple))))
    `(magit-signature-untrusted         ((t (:foreground ,gruvbox-bright_blue))))
    `(magit-tag                         ((t  (:foreground ,gruvbox-bright_yellow))))

    ;; marginalia
    `(marginalia-documentation          ((t (:italic t :foreground ,gruvbox-light3))))

    ;; markdown-mode
    `(markdown-header-face-1            ((t (:foreground ,gruvbox-neutral_blue))))
    `(markdown-header-face-2            ((t (:foreground ,gruvbox-neutral_yellow))))
    `(markdown-header-face-3            ((t (:foreground ,gruvbox-neutral_purple))))
    `(markdown-header-face-4            ((t (:foreground ,gruvbox-neutral_red))))
    `(markdown-header-face-5            ((t (:foreground ,gruvbox-neutral_green))))
    `(markdown-header-face-6            ((t (:foreground ,gruvbox-neutral_aqua))))

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
    `(org-warning                       ((t (:bold t :foreground ,gruvbox-neutral_red :weight bold :underline unspecified))))
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
    ;; `(org-habit-clear-face              ((t (:background ,gruvbox-faded_blue))))
    ;; `(org-habit-clear-future-face       ((t (:background ,gruvbox-neutral_blue))))
    ;; `(org-habit-ready-face              ((t (:background ,gruvbox-faded_green))))
    ;; `(org-habit-ready-future-face       ((t (:background ,gruvbox-neutral_green))))
    ;; `(org-habit-alert-face              ((t (:background ,gruvbox-faded_yellow))))
    ;; `(org-habit-alert-future-face       ((t (:background ,gruvbox-neutral_yellow))))
    ;; `(org-habit-overdue-face            ((t (:background ,gruvbox-faded_red))))
    ;; `(org-habit-overdue-future-face     ((t (:background ,gruvbox-neutral_red))))

    ;; popup
    `(popup-face                        ((t (:foreground ,gruvbox-light1 :background ,gruvbox-dark1))))
    `(popup-menu-mouse-face             ((t (:foreground ,gruvbox-light0 :background ,gruvbox-faded_green))))
    `(popup-menu-selection-face         ((t (:foreground ,gruvbox-light0 :background ,gruvbox-faded_green))))
    `(popup-tip-face                    ((t (:foreground ,gruvbox-light2 :background ,gruvbox-dark2))))

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
    `(rainbow-delimiters-unmatched-face ((t (:background unspecified :foreground ,gruvbox-light0))))

    ;; Smartparens
    ;; `(sp-pair-overlay-face              ((t (:background ,gruvbox-dark2))))
    ;; `(sp-wrap-overlay-face              ((t (:inherit sp-wrap-overlay-face))))
    ;; `(sp-wrap-tag-overlay-face          ((t (:inherit sp-wrap-overlay-face))))
    ;; `(sp-show-pair-match-face           ((t (:background ,gruvbox-dark2)))) ;; Pair tags highlight
    ;; `(sp-show-pair-mismatch-face        ((t (:background ,gruvbox-neutral_red)))) ;; Highlight for bracket without pair

    ;; Smart-mode-line
    `(sml/global                        ((t (:foreground ,gruvbox-burlywood4 :inverse-video unspecified))))
    `(sml/modes                         ((t (:foreground ,gruvbox-bright_green))))
    `(sml/filename                      ((t (:foreground ,gruvbox-bright_yellow :weight bold))))
    `(sml/prefix                        ((t (:foreground ,gruvbox-light1))))
    `(sml/read-only                     ((t (:foreground ,gruvbox-neutral_blue))))
    `(persp-selected-face               ((t (:foreground ,gruvbox-neutral_orange))))

    ;; web-mode
    `(web-mode-doctype-face             ((t (:foreground ,gruvbox-bright_blue))))
    `(web-mode-html-tag-bracket-face    ((t (:foreground ,gruvbox-bright_blue))))
    `(web-mode-html-tag-face            ((t (:foreground ,gruvbox-bright_blue))))
    `(web-mode-html-attr-name-face      ((t (:foreground ,gruvbox-bright_yellow))))
    `(web-mode-html-attr-equal-face     ((t (:foreground ,gruvbox-bright_yellow))))
    `(web-mode-html-attr-value-face     ((t (:foreground ,gruvbox-bright_green))))

    ;; wgrep
    `(wgrep-delete-face                 ((t (:strike-through ,gruvbox-bright_red))))
    `(wgrep-done-face                   ((t (:foreground ,gruvbox-turquoise4))))
    `(wgrep-face                        ((t (:underline (:color ,gruvbox-bright_yellow :style line)))))
    `(wgrep-file-face                   ((t (:inherit 'highlight))))
    `(wgrep-reject-face                 ((t (:foreground ,gruvbox-bright_red :bold t))))
)

(custom-theme-set-variables
  'gruvbox

  `(ansi-color-names-vector [,gruvbox-dark1 ,gruvbox-neutral_red
    ,gruvbox-neutral_green ,gruvbox-neutral_yellow ,gruvbox-neutral_blue
    ,gruvbox-neutral_purple ,gruvbox-neutral_aqua ,gruvbox-light1])))

(defun gruvbox-set-ansi-color-names-vector ()
  "Give comint and the like the same colours as the term colours we set."
  (setq-default ansi-color-names-vector
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
