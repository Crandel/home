;;; dark-mint-theme.el --- dark & minty fresh theme

;; Copyright (C) 2016 by Shaun Viguerie


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.    See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.    If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(deftheme dark-mint
  "dark-mint-theme")


(custom-theme-set-faces
 'dark-mint

 '(default ((t (:background "black" :foreground "GreenYellow"))))
 '(mouse ((t (:foregound "#000000"))))
 '(fringe ((t (:background "black" :foreground "GreenYellow"))))
 '(cursor ((t (:foregound "#000000"))))
 '(border ((t (:foregound "black"))))

 '(mode-line ((t (:background "black" :foreground "GreenYellow"))))
 '(Man-overstrike-face ((t (:weight bold))))
 '(Man-underline-face ((t (:underline t))))
 '(apropos-keybinding-face ((t (:underline t))))
 '(apropos-label-face ((t (:italic t))))

 '(font-lock-type-face ((t (:foreground "light slate blue"))))
 '(font-lock-comment-face ((t (:foreground "Grey"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "DarkGrey" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "GoldenRod" :weight bold))))
 '(font-lock-doc-face ((t (:foreground "Khaki"))))
 '(font-lock-doc-string-face ((t (:foreground "LemonChiffon"))))
 '(font-lock-function-name-face ((t (:foreground "Gold" :weight bold))))
 '(font-lock-keyword-face ((t (:weight bold :foreground "DarkGoldenrod"))))
 '(font-lock-string-face ((t (:foreground "#DF7401"))))
 '(font-lock-variable-name-face ((t (:foreground "DarkOliveGreen3"))))

 '(font-lock-negation-char-face ((t (:foreground "Indigo"))))
 '(font-lock-preprocessor-face ((t (:foreground "DarkOrange"))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "Cyan"))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "Cyan"))))
 '(font-lock-warning-face ((t (:weight bold :foreground "OrangeRed"))))


 '(region ((t (:background "SteelBlue"))))
 '(secondary-selection ((t (:background "dodger blue"))))

 '(mouse ((t (:foregound "wheat"))))
 '(highlight ((t (:background "blue" :foreground "orange"))))
 '(show-paren-match-face ((t (:background "turquoise" :foreground "coral"))))
 '(show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))
 '(cursor ((t (:background "Deep Pink")))))

'(linum ((t (:background "black" :foreground "green"))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'dark-mint)
;;; dark-mint-theme.el ends here
