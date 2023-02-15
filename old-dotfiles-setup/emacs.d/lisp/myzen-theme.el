;;; myzen-theme.el --- A low contrast color theme for Emacs.

;; Copyright (C) 2011-2018 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://github.com/bbatsov/zenburn-emacs
;; Version: 2.7-snapshot

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A port of the popular Vim theme Zenburn for Emacs 24+, built on top
;; of the new built-in theme support in Emacs 24.

;;; Credits:

;; Jani Nurminen created the original theme for vim on which this port
;; is based.

;;; Code:

(deftheme myzen-theme "A modified Zenburn color theme for Agda")

(defgroup myzen-theme nil
  "myzen theme."
  :group 'faces
  :prefix "myzen-"
  ; :link '(url-link :tag "GitHub" "http://github.com/bbatsov/zenburn-emacs")
  :tag "My Zen theme")

;;;###autoload
(defcustom myzen-override-colors-alist '()
  "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist."
  :group 'myzen-theme
  :type '(alist
          :key-type (string :tag "Name")
          :value-type (string :tag " Hex")))

(defvar myzen-use-variable-pitch nil
  "When non-nil, use variable pitch face for some headings and titles.")

(defvar myzen-scale-org-headlines nil
  "Whether `org-mode' headlines should be scaled.")

(defvar myzen-scale-outline-headlines nil
  "Whether `outline-mode' headlines should be scaled.")

(defcustom myzen-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'myzen-theme
  :package-version '(myzen . "2.6"))

(defcustom myzen-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'myzen-theme
  :package-version '(myzen . "2.6"))

(defcustom myzen-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'myzen-theme
  :package-version '(myzen . "2.6"))

(defcustom myzen-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'myzen-theme
  :package-version '(myzen . "2.6"))

(defcustom myzen-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'myzen-theme
  :package-version '(myzen . "2.6"))

;;; Color Palette

(defvar myzen-default-colors-alist
  '(("myzen-fg-1"     . "#656555")
    ("myzen-fg-05"    . "#989890")
    ("myzen-fg"       . "#DCDCCC")
    ("myzen-fg+1"     . "#FFFFEF")
    ("myzen-fg+2"     . "#FFFFFD")
    ("myzen-bg-2"     . "#000000")
    ("myzen-bg-1"     . "#2B2B2B")
    ("myzen-bg-08"    . "#303030")
    ("myzen-bg-05"    . "#383838")
    ("myzen-bg"       . "#3F3F3F")
    ("myzen-bg+05"    . "#494949")
    ("myzen-bg+1"     . "#4F4F4F")
    ("myzen-bg+2"     . "#5F5F5F")
    ("myzen-bg+3"     . "#6F6F6F")
    ("myzen-red-6"    . "#6C3333")
    ("myzen-red-5"    . "#7C4343")
    ("myzen-red-4"    . "#8C5353")
    ("myzen-red-3"    . "#9C6363")
    ("myzen-red-2"    . "#AC7373")
    ("myzen-red-1"    . "#BC8383")
    ("myzen-red"      . "#CC9393")
    ("myzen-red+1"    . "#DCA3A3")
    ("myzen-red+2"    . "#ECB3B3")
    ("myzen-orange"   . "#DFAF8F")
    ("myzen-yellow-2" . "#D0BF8F")
    ("myzen-yellow-1" . "#E0CF9F")
    ("myzen-yellow"   . "#F0DFAF")
    ("myzen-green-5"  . "#2F4F2F")
    ("myzen-green-4"  . "#3F5F3F")
    ("myzen-green-3"  . "#4F6F4F")
    ("myzen-green-2"  . "#5F7F5F")
    ("myzen-green-1"  . "#6F8F6F")
    ("myzen-green"    . "#7F9F7F")
    ("myzen-green+1"  . "#8FB28F")
    ("myzen-green+2"  . "#9FC59F")
    ("myzen-green+3"  . "#AFD8AF")
    ("myzen-green+4"  . "#BFEBBF")
    ("myzen-cyan"     . "#93E0E3")
    ("myzen-blue+3"   . "#BDE0F3")
    ("myzen-blue+2"   . "#ACE0E3")
    ("myzen-blue+1"   . "#94BFF3")
    ("myzen-blue"     . "#8CD0D3")
    ("myzen-blue-1"   . "#7CB8BB")
    ("myzen-blue-2"   . "#6CA0A3")
    ("myzen-blue-3"   . "#5C888B")
    ("myzen-blue-4"   . "#4C7073")
    ("myzen-blue-5"   . "#366060")
    ("myzen-magenta"  . "#DC8CC3"))
  "List of myzen colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro myzen-with-color-variables (&rest body)
  "`let' bind all colors defined in `myzen-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   (append myzen-default-colors-alist
                           myzen-override-colors-alist))
         (z-variable-pitch (if myzen-use-variable-pitch
                               'variable-pitch 'default)))
     ,@body))

;;; Theme Faces
(myzen-with-color-variables
  (custom-theme-set-faces
   'myzen
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,myzen-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,myzen-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,myzen-fg :background ,myzen-bg))))
   `(cursor ((t (:foreground ,myzen-fg :background ,myzen-fg+1))))
   `(widget-field ((t (:foreground ,myzen-fg :background ,myzen-bg+3))))
   `(escape-glyph ((t (:foreground ,myzen-yellow :weight bold))))
   `(fringe ((t (:foreground ,myzen-fg :background ,myzen-bg+1))))
   `(header-line ((t (:foreground ,myzen-yellow
                                  :background ,myzen-bg-1
                                  :box (:line-width -1 :style released-button)
                                  :extend t))))
   `(highlight ((t (:background ,myzen-bg-05))))
   `(success ((t (:foreground ,myzen-green :weight bold))))
   `(warning ((t (:foreground ,myzen-orange :weight bold))))
   `(tooltip ((t (:foreground ,myzen-fg :background ,myzen-bg+1))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,myzen-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,myzen-green))))
   `(compilation-error-face ((t (:foreground ,myzen-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,myzen-fg))))
   `(compilation-info-face ((t (:foreground ,myzen-blue))))
   `(compilation-info ((t (:foreground ,myzen-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,myzen-green))))
   `(compilation-line-face ((t (:foreground ,myzen-yellow))))
   `(compilation-line-number ((t (:foreground ,myzen-yellow))))
   `(compilation-message-face ((t (:foreground ,myzen-blue))))
   `(compilation-warning-face ((t (:foreground ,myzen-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,myzen-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,myzen-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,myzen-yellow :weight bold))))
;;;;; completions
   `(completions-annotations ((t (:foreground ,myzen-fg-1))))
;;;;; customize
   `(custom-variable-tag ((t (:foreground ,myzen-blue :weight bold))))
   `(custom-group-tag ((t (:foreground ,myzen-blue :weight bold :height 1.2))))
   `(custom-state ((t (:foreground ,myzen-green+4))))
;;;;; display-fill-column-indicator
     `(fill-column-indicator ((,class :foreground ,myzen-bg-05 :weight semilight)))
;;;;; eww
   '(eww-invalid-certificate ((t (:inherit error))))
   '(eww-valid-certificate   ((t (:inherit success))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,myzen-fg))))
   `(grep-error-face ((t (:foreground ,myzen-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,myzen-blue))))
   `(grep-match-face ((t (:foreground ,myzen-orange :weight bold))))
   `(match ((t (:background ,myzen-bg-1 :foreground ,myzen-orange :weight bold))))
;;;;; hi-lock
   `(hi-blue    ((t (:background ,myzen-cyan    :foreground ,myzen-bg-1))))
   `(hi-green   ((t (:background ,myzen-green+4 :foreground ,myzen-bg-1))))
   `(hi-pink    ((t (:background ,myzen-magenta :foreground ,myzen-bg-1))))
   `(hi-yellow  ((t (:background ,myzen-yellow  :foreground ,myzen-bg-1))))
   `(hi-blue-b  ((t (:foreground ,myzen-blue    :weight     bold))))
   `(hi-green-b ((t (:foreground ,myzen-green+2 :weight     bold))))
   `(hi-red-b   ((t (:foreground ,myzen-red     :weight     bold))))
;;;;; info
   `(Info-quoted ((t (:inherit font-lock-constant-face))))
;;;;; isearch
   `(isearch ((t (:foreground ,myzen-yellow-2 :weight bold :background ,myzen-bg+2))))
   `(isearch-fail ((t (:foreground ,myzen-fg :background ,myzen-red-4))))
   `(lazy-highlight ((t (:foreground ,myzen-yellow-2 :weight bold :background ,myzen-bg-05))))

   `(menu ((t (:foreground ,myzen-fg :background ,myzen-bg))))
   `(minibuffer-prompt ((t (:foreground ,myzen-yellow))))
   `(mode-line
     ((,class (:foreground ,myzen-green+1
                           :background ,myzen-bg-1
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,myzen-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,myzen-green-2
                      :background ,myzen-bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,myzen-bg-1 :extend t))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,myzen-bg+2))))
   `(trailing-whitespace ((t (:background ,myzen-red))))
   `(vertical-border ((t (:foreground ,myzen-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,myzen-fg :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,myzen-green))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,myzen-green-2))))
   `(font-lock-constant-face ((t (:foreground ,myzen-green+4))))
   `(font-lock-doc-face ((t (:foreground ,myzen-green+2))))
   `(font-lock-function-name-face ((t (:foreground ,myzen-cyan))))
   `(font-lock-keyword-face ((t (:foreground ,myzen-yellow :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,myzen-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,myzen-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,myzen-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,myzen-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,myzen-red))))
   `(font-lock-type-face ((t (:foreground ,myzen-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,myzen-orange))))
   `(font-lock-warning-face ((t (:foreground ,myzen-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; line numbers (Emacs 26.1 and above)
   `(line-number ((t (:foreground ,myzen-bg+3 :background ,myzen-bg-05))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,myzen-yellow-2))))
;;;;; man
   '(Man-overstrike ((t (:inherit font-lock-keyword-face))))
   '(Man-underline  ((t (:inherit (font-lock-string-face underline)))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,myzen-fg))))
   `(newsticker-default-face ((t (:foreground ,myzen-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,myzen-green+3))))
   `(newsticker-extra-face ((t (:foreground ,myzen-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,myzen-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,myzen-green))))
   `(newsticker-new-item-face ((t (:foreground ,myzen-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,myzen-red))))
   `(newsticker-old-item-face ((t (:foreground ,myzen-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,myzen-fg))))
   `(newsticker-treeview-face ((t (:foreground ,myzen-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,myzen-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,myzen-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,myzen-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,myzen-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,myzen-bg+3))))
   `(newsticker-treeview-selection-face ((t (:background ,myzen-bg-1 :foreground ,myzen-yellow))))
;;;;; woman
   '(woman-bold   ((t (:inherit font-lock-keyword-face))))
   '(woman-italic ((t (:inherit (font-lock-string-face italic)))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,myzen-fg-1 :background ,myzen-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,myzen-green+2 :background ,myzen-bg :inverse-video nil))))
;;;;; ace-window
   `(aw-background-face
     ((t (:foreground ,myzen-fg-1 :background ,myzen-bg :inverse-video nil))))
   `(aw-leading-char-face ((t (:inherit aw-mode-line-face))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,myzen-green+1))))
   `(android-mode-error-face ((t (:foreground ,myzen-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,myzen-fg))))
   `(android-mode-verbose-face ((t (:foreground ,myzen-green))))
   `(android-mode-warning-face ((t (:foreground ,myzen-yellow))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,myzen-cyan :weight bold))))
   `(anzu-mode-line-no-match ((t (:foreground ,myzen-red :weight bold))))
   `(anzu-match-1 ((t (:foreground ,myzen-bg :background ,myzen-green))))
   `(anzu-match-2 ((t (:foreground ,myzen-bg :background ,myzen-orange))))
   `(anzu-match-3 ((t (:foreground ,myzen-bg :background ,myzen-blue))))
   `(anzu-replace-to ((t (:inherit anzu-replace-highlight :foreground ,myzen-yellow))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,myzen-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,myzen-yellow))))
   `(font-latex-italic-face ((t (:foreground ,myzen-cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,myzen-orange))))
   `(font-latex-script-char-face ((t (:foreground ,myzen-orange))))
;;;;; agda-mode
   `(agda2-highlight-keyword-face ((t (:foreground ,myzen-yellow :weight bold))))
   `(agda2-highlight-string-face ((t (:foreground ,myzen-red))))
   `(agda2-highlight-symbol-face ((t (:foreground ,myzen-orange))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,myzen-blue-1))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,myzen-fg))))
   `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,myzen-fg))))
   `(agda2-highlight-datatype-face ((t (:foreground ,myzen-blue))))
   `(agda2-highlight-function-face ((t (:foreground ,myzen-blue))))
   `(agda2-highlight-module-face ((t (:foreground ,myzen-blue-1))))
   `(agda2-highlight-error-face ((t (:foreground ,myzen-bg :background ,myzen-magenta))))
   `(agda2-highlight-unsolved-meta-face ((t (:foreground ,myzen-bg :background ,myzen-magenta))))
   `(agda2-highlight-unsolved-constraint-face ((t (:foreground ,myzen-bg :background ,myzen-magenta))))
   `(agda2-highlight-termination-problem-face ((t (:foreground ,myzen-bg :background ,myzen-magenta))))
   `(agda2-highlight-incomplete-pattern-face ((t (:foreground ,myzen-bg :background ,myzen-magenta))))
   `(agda2-highlight-typechecks-face ((t (:background ,myzen-red-4))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,myzen-bg+3 :foreground ,myzen-bg-2))))
   `(ac-selection-face ((t (:background ,myzen-blue-4 :foreground ,myzen-fg))))
   `(popup-tip-face ((t (:background ,myzen-yellow-2 :foreground ,myzen-bg-2))))
   `(popup-menu-mouse-face ((t (:background ,myzen-yellow-2 :foreground ,myzen-bg-2))))
   `(popup-summary-face ((t (:background ,myzen-bg+3 :foreground ,myzen-bg-2))))
   `(popup-scroll-bar-foreground-face ((t (:background ,myzen-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,myzen-bg-1))))
   `(popup-isearch-match ((t (:background ,myzen-bg :foreground ,myzen-fg))))
;;;;; avy
   `(avy-background-face
     ((t (:foreground ,myzen-fg-1 :background ,myzen-bg :inverse-video nil))))
   `(avy-lead-face-0
     ((t (:foreground ,myzen-green+3 :background ,myzen-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-1
     ((t (:foreground ,myzen-yellow :background ,myzen-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-2
     ((t (:foreground ,myzen-red+1 :background ,myzen-bg :inverse-video nil :weight bold))))
   `(avy-lead-face
     ((t (:foreground ,myzen-cyan :background ,myzen-bg :inverse-video nil :weight bold))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,myzen-fg :background ,myzen-bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,myzen-orange :background ,myzen-bg+1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,myzen-orange :background ,myzen-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,myzen-fg :background ,myzen-bg-1))))
   `(company-tooltip-mouse ((t (:background ,myzen-bg-1))))
   `(company-tooltip-common ((t (:foreground ,myzen-green+2))))
   `(company-tooltip-common-selection ((t (:foreground ,myzen-green+2))))
   `(company-scrollbar-fg ((t (:background ,myzen-bg-1))))
   `(company-scrollbar-bg ((t (:background ,myzen-bg+2))))
   `(company-preview ((t (:background ,myzen-green+2))))
   `(company-preview-common ((t (:foreground ,myzen-green+2 :background ,myzen-bg-1))))
;;;;; bm
   `(bm-face ((t (:background ,myzen-yellow-1 :foreground ,myzen-bg))))
   `(bm-fringe-face ((t (:background ,myzen-yellow-1 :foreground ,myzen-bg))))
   `(bm-fringe-persistent-face ((t (:background ,myzen-green-2 :foreground ,myzen-bg))))
   `(bm-persistent-face ((t (:background ,myzen-green-2 :foreground ,myzen-bg))))
;;;;; calfw
   `(cfw:face-annotation ((t (:foreground ,myzen-red :inherit cfw:face-day-title))))
   `(cfw:face-day-title ((t nil)))
   `(cfw:face-default-content ((t (:foreground ,myzen-green))))
   `(cfw:face-default-day ((t (:weight bold))))
   `(cfw:face-disable ((t (:foreground ,myzen-fg-1))))
   `(cfw:face-grid ((t (:inherit shadow))))
   `(cfw:face-header ((t (:inherit font-lock-keyword-face))))
   `(cfw:face-holiday ((t (:inherit cfw:face-sunday))))
   `(cfw:face-periods ((t (:foreground ,myzen-cyan))))
   `(cfw:face-saturday ((t (:foreground ,myzen-blue :weight bold))))
   `(cfw:face-select ((t (:background ,myzen-blue-5))))
   `(cfw:face-sunday ((t (:foreground ,myzen-red :weight bold))))
   `(cfw:face-title ((t (:height 2.0 :inherit (variable-pitch font-lock-keyword-face)))))
   `(cfw:face-today ((t (:foreground ,myzen-cyan :weight bold))))
   `(cfw:face-today-title ((t (:inherit highlight bold))))
   `(cfw:face-toolbar ((t (:background ,myzen-blue-5))))
   `(cfw:face-toolbar-button-off ((t (:underline nil :inherit link))))
   `(cfw:face-toolbar-button-on ((t (:underline nil :inherit link-visited))))
;;;;; centaur-tabs
   `(centaur-tabs-default ((t (:background ,myzen-bg :foreground ,myzen-fg :box nil))))
   `(centaur-tabs-selected ((t (:background ,myzen-bg :foreground ,myzen-fg+2 :box nil))))
   `(centaur-tabs-unselected ((t (:background ,myzen-bg-1 :foreground ,myzen-fg-05 :box nil))))
   `(centaur-tabs-selected-modified ((t (:background ,myzen-bg :foreground ,myzen-orange :box nil))))
   `(centaur-tabs-unselected-modified ((t (:background ,myzen-bg-1 :foreground ,myzen-orange :box nil))))
   `(centaur-tabs-active-bar-face ((t (:background ,myzen-yellow :box nil))))
   `(centaur-tabs-modified-marker-selected ((t (:inherit 'centaur-tabs-selected-modified :foreground ,myzen-yellow :box nil))))
   `(centaur-tabs-modified-marker-unselected ((t (:inherit 'centaur-tabs-unselected-modified :foreground ,myzen-yellow :box nil))))
;;;;; cider
   `(cider-result-overlay-face ((t (:background unspecified))))
   `(cider-enlightened-face ((t (:box (:color ,myzen-orange :line-width -1)))))
   `(cider-enlightened-local-face ((t (:weight bold :foreground ,myzen-green+1))))
   `(cider-deprecated-face ((t (:background ,myzen-yellow-2))))
   `(cider-instrumented-face ((t (:box (:color ,myzen-red :line-width -1)))))
   `(cider-traced-face ((t (:box (:color ,myzen-cyan :line-width -1)))))
   `(cider-test-failure-face ((t (:background ,myzen-red-4))))
   `(cider-test-error-face ((t (:background ,myzen-magenta))))
   `(cider-test-success-face ((t (:background ,myzen-green-2))))
   `(cider-fringe-good-face ((t (:foreground ,myzen-green+4))))
;;;;; circe
   `(circe-highlight-nick-face ((t (:foreground ,myzen-cyan))))
   `(circe-my-message-face ((t (:foreground ,myzen-fg))))
   `(circe-fool-face ((t (:foreground ,myzen-red+1))))
   `(circe-topic-diff-removed-face ((t (:foreground ,myzen-red :weight bold))))
   `(circe-originator-face ((t (:foreground ,myzen-fg))))
   `(circe-server-face ((t (:foreground ,myzen-green))))
   `(circe-topic-diff-new-face ((t (:foreground ,myzen-orange :weight bold))))
   `(circe-prompt-face ((t (:foreground ,myzen-orange :background ,myzen-bg :weight bold))))
;;;;; context-coloring
   `(context-coloring-level-0-face ((t :foreground ,myzen-fg)))
   `(context-coloring-level-1-face ((t :foreground ,myzen-cyan)))
   `(context-coloring-level-2-face ((t :foreground ,myzen-green+4)))
   `(context-coloring-level-3-face ((t :foreground ,myzen-yellow)))
   `(context-coloring-level-4-face ((t :foreground ,myzen-orange)))
   `(context-coloring-level-5-face ((t :foreground ,myzen-magenta)))
   `(context-coloring-level-6-face ((t :foreground ,myzen-blue+1)))
   `(context-coloring-level-7-face ((t :foreground ,myzen-green+2)))
   `(context-coloring-level-8-face ((t :foreground ,myzen-yellow-2)))
   `(context-coloring-level-9-face ((t :foreground ,myzen-red+1)))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,myzen-blue :foreground ,myzen-bg))))
   `(ctbl:face-continue-bar ((t (:background ,myzen-bg-05 :foreground ,myzen-bg))))
   `(ctbl:face-row-select ((t (:background ,myzen-cyan :foreground ,myzen-bg))))
;;;;; debbugs
   `(debbugs-gnu-done ((t (:foreground ,myzen-fg-1))))
   `(debbugs-gnu-handled ((t (:foreground ,myzen-green))))
   `(debbugs-gnu-new ((t (:foreground ,myzen-red))))
   `(debbugs-gnu-pending ((t (:foreground ,myzen-blue))))
   `(debbugs-gnu-stale ((t (:foreground ,myzen-orange))))
   `(debbugs-gnu-tagged ((t (:foreground ,myzen-red))))
;;;;; diff
   ;; Please read (info "(magit)Theming Faces") before changing this.
   `(diff-added          ((t (:background "#335533" :foreground ,myzen-green))))
   `(diff-changed        ((t (:background "#555511" :foreground ,myzen-yellow-1))))
   `(diff-removed        ((t (:background "#553333" :foreground ,myzen-red-2))))
   `(diff-refine-added   ((t (:background "#338833" :foreground ,myzen-green+4))))
   `(diff-refine-changed ((t (:background "#888811" :foreground ,myzen-yellow))))
   `(diff-refine-removed ((t (:background "#883333" :foreground ,myzen-red))))
   `(diff-header ((,class (:background ,myzen-bg+2))
                  (t (:background ,myzen-fg :foreground ,myzen-bg))))
   `(diff-file-header
     ((,class (:background ,myzen-bg+2 :foreground ,myzen-fg :weight bold))
      (t (:background ,myzen-fg :foreground ,myzen-bg :weight bold))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,myzen-blue :background ,myzen-blue-2))))
   `(diff-hl-delete ((,class (:foreground ,myzen-red+1 :background ,myzen-red-1))))
   `(diff-hl-insert ((,class (:foreground ,myzen-green+1 :background ,myzen-green-2))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,myzen-bg+1)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,myzen-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,myzen-orange))))
   `(diredp-date-time ((t (:foreground ,myzen-magenta))))
   `(diredp-deletion ((t (:foreground ,myzen-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,myzen-red))))
   `(diredp-dir-heading ((t (:foreground ,myzen-blue :background ,myzen-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,myzen-cyan))))
   `(diredp-exec-priv ((t (:foreground ,myzen-red))))
   `(diredp-executable-tag ((t (:foreground ,myzen-green+1))))
   `(diredp-file-name ((t (:foreground ,myzen-blue))))
   `(diredp-file-suffix ((t (:foreground ,myzen-green))))
   `(diredp-flag-mark ((t (:foreground ,myzen-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,myzen-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,myzen-red))))
   `(diredp-link-priv ((t (:foreground ,myzen-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,myzen-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,myzen-orange))))
   `(diredp-no-priv ((t (:foreground ,myzen-fg))))
   `(diredp-number ((t (:foreground ,myzen-green+1))))
   `(diredp-other-priv ((t (:foreground ,myzen-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,myzen-red-1))))
   `(diredp-read-priv ((t (:foreground ,myzen-green-2))))
   `(diredp-symlink ((t (:foreground ,myzen-yellow))))
   `(diredp-write-priv ((t (:foreground ,myzen-magenta))))
;;;;; dired-async
   `(dired-async-failures ((t (:foreground ,myzen-red :weight bold))))
   `(dired-async-message ((t (:foreground ,myzen-yellow :weight bold))))
   `(dired-async-mode-message ((t (:foreground ,myzen-yellow))))
;;;;; diredfl
   `(diredfl-compressed-file-suffix ((t (:foreground ,myzen-orange))))
   `(diredfl-date-time ((t (:foreground ,myzen-magenta))))
   `(diredfl-deletion ((t (:foreground ,myzen-yellow))))
   `(diredfl-deletion-file-name ((t (:foreground ,myzen-red))))
   `(diredfl-dir-heading ((t (:foreground ,myzen-blue :background ,myzen-bg-1))))
   `(diredfl-dir-priv ((t (:foreground ,myzen-cyan))))
   `(diredfl-exec-priv ((t (:foreground ,myzen-red))))
   `(diredfl-executable-tag ((t (:foreground ,myzen-green+1))))
   `(diredfl-file-name ((t (:foreground ,myzen-blue))))
   `(diredfl-file-suffix ((t (:foreground ,myzen-green))))
   `(diredfl-flag-mark ((t (:foreground ,myzen-yellow))))
   `(diredfl-flag-mark-line ((t (:foreground ,myzen-orange))))
   `(diredfl-ignored-file-name ((t (:foreground ,myzen-red))))
   `(diredfl-link-priv ((t (:foreground ,myzen-yellow))))
   `(diredfl-no-priv ((t (:foreground ,myzen-fg))))
   `(diredfl-number ((t (:foreground ,myzen-green+1))))
   `(diredfl-other-priv ((t (:foreground ,myzen-yellow-1))))
   `(diredfl-rare-priv ((t (:foreground ,myzen-red-1))))
   `(diredfl-read-priv ((t (:foreground ,myzen-green-1))))
   `(diredfl-symlink ((t (:foreground ,myzen-yellow))))
   `(diredfl-write-priv ((t (:foreground ,myzen-magenta))))
;;;;; doom-modeline
   `(doom-modeline-bar  ((t (:background ,myzen-yellow))))
   `(doom-modeline-inactive-bar  ((t (:background nil))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,myzen-fg :background ,myzen-red-4))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,myzen-fg :background ,myzen-red-4))))
   `(ediff-current-diff-B ((t (:foreground ,myzen-fg :background ,myzen-green-2))))
   `(ediff-current-diff-C ((t (:foreground ,myzen-fg :background ,myzen-blue-5))))
   `(ediff-even-diff-A ((t (:background ,myzen-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,myzen-bg+1))))
   `(ediff-even-diff-B ((t (:background ,myzen-bg+1))))
   `(ediff-even-diff-C ((t (:background ,myzen-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,myzen-fg :background ,myzen-red-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,myzen-fg :background ,myzen-red-2 weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,myzen-fg :background ,myzen-green :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,myzen-fg :background ,myzen-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,myzen-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,myzen-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,myzen-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,myzen-bg+2))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,myzen-fg))))
   `(egg-help-header-1 ((t (:foreground ,myzen-yellow))))
   `(egg-help-header-2 ((t (:foreground ,myzen-green+3))))
   `(egg-branch ((t (:foreground ,myzen-yellow))))
   `(egg-branch-mono ((t (:foreground ,myzen-yellow))))
   `(egg-term ((t (:foreground ,myzen-yellow))))
   `(egg-diff-add ((t (:foreground ,myzen-green+4))))
   `(egg-diff-del ((t (:foreground ,myzen-red+1))))
   `(egg-diff-file-header ((t (:foreground ,myzen-yellow-2))))
   `(egg-section-title ((t (:foreground ,myzen-yellow))))
   `(egg-stash-mono ((t (:foreground ,myzen-green+4))))
;;;;; elfeed
   `(elfeed-log-error-level-face ((t (:foreground ,myzen-red))))
   `(elfeed-log-info-level-face ((t (:foreground ,myzen-blue))))
   `(elfeed-log-warn-level-face ((t (:foreground ,myzen-yellow))))
   `(elfeed-search-date-face ((t (:foreground ,myzen-yellow-1 :underline t
                                              :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,myzen-green))))
   `(elfeed-search-feed-face ((t (:foreground ,myzen-cyan))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,myzen-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,myzen-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,myzen-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,myzen-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,myzen-green+2 :background ,myzen-bg))))
   `(w3m-lnum-match ((t (:background ,myzen-bg-1
                                     :foreground ,myzen-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,myzen-yellow))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,myzen-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,myzen-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default-face))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default-face))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,myzen-yellow))))
   `(erc-keyword-face ((t (:foreground ,myzen-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,myzen-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,myzen-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default-face))))
   `(erc-notice-face ((t (:foreground ,myzen-green))))
   `(erc-pal-face ((t (:foreground ,myzen-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,myzen-orange :background ,myzen-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,myzen-green+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; eros
   `(eros-result-overlay-face ((t (:background unspecified))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,myzen-green+4 :background ,myzen-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,myzen-red :background ,myzen-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,myzen-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,myzen-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,myzen-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,myzen-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,myzen-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,myzen-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,myzen-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,myzen-green+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,myzen-red-1) :inherit unspecified))
      (t (:foreground ,myzen-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,myzen-yellow) :inherit unspecified))
      (t (:foreground ,myzen-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,myzen-cyan) :inherit unspecified))
      (t (:foreground ,myzen-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,myzen-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,myzen-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,myzen-cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,myzen-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,myzen-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,myzen-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,myzen-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,myzen-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,myzen-green-2 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,myzen-orange) :inherit unspecified))
      (t (:foreground ,myzen-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,myzen-red) :inherit unspecified))
      (t (:foreground ,myzen-red-1 :weight bold :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,myzen-fg))))
   `(ack-file ((t (:foreground ,myzen-blue))))
   `(ack-line ((t (:foreground ,myzen-yellow))))
   `(ack-match ((t (:foreground ,myzen-orange :background ,myzen-bg-1 :weight bold))))
;;;;; git-annex
   '(git-annex-dired-annexed-available ((t (:inherit success :weight normal))))
   '(git-annex-dired-annexed-unavailable ((t (:inherit error :weight normal))))
;;;;; git-commit
   `(git-commit-comment-action  ((,class (:foreground ,myzen-green+1 :weight bold))))
   `(git-commit-comment-branch  ((,class (:foreground ,myzen-blue+1  :weight bold)))) ; obsolete
   `(git-commit-comment-branch-local  ((,class (:foreground ,myzen-blue+1  :weight bold))))
   `(git-commit-comment-branch-remote ((,class (:foreground ,myzen-green  :weight bold))))
   `(git-commit-comment-heading ((,class (:foreground ,myzen-yellow  :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,myzen-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,myzen-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,myzen-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,myzen-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,myzen-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,myzen-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,myzen-magenta :weight bold))))
;;;;; git-rebase
   `(git-rebase-hash ((t (:foreground, myzen-orange))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:weight bold :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:weight bold :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:weight bold :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:weight bold :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:weight bold :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:weight bold :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:weight bold :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:weight bold :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:weight bold :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:weight bold :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:weight bold :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:weight bold :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:weight bold :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:weight bold :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-to))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-server-opened ((t (:foreground ,myzen-green+2 :weight bold))))
   `(gnus-server-denied ((t (:foreground ,myzen-red+1 :weight bold))))
   `(gnus-server-closed ((t (:foreground ,myzen-blue :slant italic))))
   `(gnus-server-offline ((t (:foreground ,myzen-yellow :weight bold))))
   `(gnus-server-agent ((t (:foreground ,myzen-blue :weight bold))))
   `(gnus-summary-cancelled ((t (:foreground ,myzen-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,myzen-blue))))
   `(gnus-summary-high-read ((t (:foreground ,myzen-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,myzen-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,myzen-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,myzen-blue))))
   `(gnus-summary-low-read ((t (:foreground ,myzen-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,myzen-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,myzen-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,myzen-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,myzen-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,myzen-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,myzen-fg))))
   `(gnus-summary-selected ((t (:foreground ,myzen-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,myzen-blue))))
   `(gnus-cite-10 ((t (:foreground ,myzen-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,myzen-yellow))))
   `(gnus-cite-2 ((t (:foreground ,myzen-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,myzen-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,myzen-green+2))))
   `(gnus-cite-5 ((t (:foreground ,myzen-green+1))))
   `(gnus-cite-6 ((t (:foreground ,myzen-green))))
   `(gnus-cite-7 ((t (:foreground ,myzen-red))))
   `(gnus-cite-8 ((t (:foreground ,myzen-red-1))))
   `(gnus-cite-9 ((t (:foreground ,myzen-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,myzen-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,myzen-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,myzen-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,myzen-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,myzen-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,myzen-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,myzen-bg+2))))
   `(gnus-signature ((t (:foreground ,myzen-yellow))))
   `(gnus-x ((t (:background ,myzen-fg :foreground ,myzen-bg))))
   `(mm-uu-extract ((t (:background ,myzen-bg-05 :foreground ,myzen-green+1))))
;;;;; go-guru
   `(go-guru-hl-identifier-face ((t (:foreground ,myzen-bg-1 :background ,myzen-green+1))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,myzen-blue))))
   `(guide-key/key-face ((t (:foreground ,myzen-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,myzen-green+1))))
;;;;; hackernews
   '(hackernews-comment-count ((t (:inherit link-visited :underline nil))))
   '(hackernews-link          ((t (:inherit link         :underline nil))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,myzen-green
                      :background ,myzen-bg
                      :underline nil
                      :box nil
                      :extend t))))
   `(helm-source-header
     ((t (:foreground ,myzen-yellow
                      :background ,myzen-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)
                      :extend t))))
   `(helm-selection ((t (:background ,myzen-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,myzen-bg+1))))
   `(helm-visible-mark ((t (:foreground ,myzen-bg :background ,myzen-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,myzen-green+4 :background ,myzen-bg-1))))
   `(helm-separator ((t (:foreground ,myzen-red :background ,myzen-bg))))
   `(helm-time-zone-current ((t (:foreground ,myzen-green+2 :background ,myzen-bg))))
   `(helm-time-zone-home ((t (:foreground ,myzen-red :background ,myzen-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,myzen-orange :background ,myzen-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,myzen-magenta :background ,myzen-bg))))
   `(helm-bookmark-info ((t (:foreground ,myzen-green+2 :background ,myzen-bg))))
   `(helm-bookmark-man ((t (:foreground ,myzen-yellow :background ,myzen-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,myzen-magenta :background ,myzen-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,myzen-red :background ,myzen-bg))))
   `(helm-buffer-process ((t (:foreground ,myzen-cyan :background ,myzen-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,myzen-fg :background ,myzen-bg))))
   `(helm-buffer-size ((t (:foreground ,myzen-fg-1 :background ,myzen-bg))))
   `(helm-ff-directory ((t (:foreground ,myzen-cyan :background ,myzen-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,myzen-fg :background ,myzen-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,myzen-green+2 :background ,myzen-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,myzen-red :background ,myzen-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,myzen-yellow :background ,myzen-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,myzen-bg :background ,myzen-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,myzen-cyan :background ,myzen-bg))))
   `(helm-grep-file ((t (:foreground ,myzen-fg :background ,myzen-bg))))
   `(helm-grep-finish ((t (:foreground ,myzen-green+2 :background ,myzen-bg))))
   `(helm-grep-lineno ((t (:foreground ,myzen-fg-1 :background ,myzen-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,myzen-red :background ,myzen-bg))))
   `(helm-match ((t (:foreground ,myzen-orange :background ,myzen-bg-1 :weight bold))))
   `(helm-moccur-buffer ((t (:foreground ,myzen-cyan :background ,myzen-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,myzen-fg-1 :background ,myzen-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,myzen-fg :background ,myzen-bg))))
;;;;; helm-lxc
   `(helm-lxc-face-frozen ((t (:foreground ,myzen-blue :background ,myzen-bg))))
   `(helm-lxc-face-running ((t (:foreground ,myzen-green :background ,myzen-bg))))
   `(helm-lxc-face-stopped ((t (:foreground ,myzen-red :background ,myzen-bg))))
;;;;; helm-swoop
   `(helm-swoop-target-line-face ((t (:foreground ,myzen-fg :background ,myzen-bg+1))))
   `(helm-swoop-target-word-face ((t (:foreground ,myzen-yellow :background ,myzen-bg+2 :weight bold))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,myzen-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,myzen-bg-05 :extend t)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,myzen-bg+1))
                   (t :weight bold)))
;;;;; hydra
   `(hydra-face-red ((t (:foreground ,myzen-red-1 :background ,myzen-bg))))
   `(hydra-face-amaranth ((t (:foreground ,myzen-red-3 :background ,myzen-bg))))
   `(hydra-face-blue ((t (:foreground ,myzen-blue :background ,myzen-bg))))
   `(hydra-face-pink ((t (:foreground ,myzen-magenta :background ,myzen-bg))))
   `(hydra-face-teal ((t (:foreground ,myzen-cyan :background ,myzen-bg))))
;;;;; info+
   `(info-command-ref-item ((t (:background ,myzen-bg-1 :foreground ,myzen-orange))))
   `(info-constant-ref-item ((t (:background ,myzen-bg-1 :foreground ,myzen-magenta))))
   `(info-double-quoted-name ((t (:inherit font-lock-comment-face))))
   `(info-file ((t (:background ,myzen-bg-1 :foreground ,myzen-yellow))))
   `(info-function-ref-item ((t (:background ,myzen-bg-1 :inherit font-lock-function-name-face))))
   `(info-macro-ref-item ((t (:background ,myzen-bg-1 :foreground ,myzen-yellow))))
   `(info-menu ((t (:foreground ,myzen-yellow))))
   `(info-quoted-name ((t (:inherit font-lock-constant-face))))
   `(info-reference-item ((t (:background ,myzen-bg-1))))
   `(info-single-quote ((t (:inherit font-lock-keyword-face))))
   `(info-special-form-ref-item ((t (:background ,myzen-bg-1 :foreground ,myzen-yellow))))
   `(info-string ((t (:inherit font-lock-string-face))))
   `(info-syntax-class-item ((t (:background ,myzen-bg-1 :foreground ,myzen-blue+1))))
   `(info-user-option-ref-item ((t (:background ,myzen-bg-1 :foreground ,myzen-red))))
   `(info-variable-ref-item ((t (:background ,myzen-bg-1 :foreground ,myzen-orange))))
;;;;; irfc
   `(irfc-head-name-face ((t (:foreground ,myzen-red :weight bold))))
   `(irfc-head-number-face ((t (:foreground ,myzen-red :weight bold))))
   `(irfc-reference-face ((t (:foreground ,myzen-blue-1 :weight bold))))
   `(irfc-requirement-keyword-face ((t (:inherit font-lock-keyword-face))))
   `(irfc-rfc-link-face ((t (:inherit link))))
   `(irfc-rfc-number-face ((t (:foreground ,myzen-cyan :weight bold))))
   `(irfc-std-number-face ((t (:foreground ,myzen-green+4 :weight bold))))
   `(irfc-table-item-face ((t (:foreground ,myzen-green+3))))
   `(irfc-title-face ((t (:foreground ,myzen-yellow
                                      :underline t :weight bold))))
;;;;; ivy
   `(ivy-confirm-face ((t (:foreground ,myzen-green :background ,myzen-bg))))
   `(ivy-current-match ((t (:foreground ,myzen-yellow :weight bold :underline t))))
   `(ivy-cursor ((t (:foreground ,myzen-bg :background ,myzen-fg))))
   `(ivy-match-required-face ((t (:foreground ,myzen-red :background ,myzen-bg))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,myzen-bg+1))))
   `(ivy-minibuffer-match-face-2 ((t (:background ,myzen-green-2))))
   `(ivy-minibuffer-match-face-3 ((t (:background ,myzen-green))))
   `(ivy-minibuffer-match-face-4 ((t (:background ,myzen-green+1))))
   `(ivy-remote ((t (:foreground ,myzen-blue :background ,myzen-bg))))
   `(ivy-subdir ((t (:foreground ,myzen-yellow :background ,myzen-bg))))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,myzen-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,myzen-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,myzen-yellow))))
   `(ido-indicator ((t (:foreground ,myzen-yellow :background ,myzen-red-4))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,myzen-bg+2 :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,myzen-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,myzen-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,myzen-red+1))))
   `(jabber-roster-user-xa ((t (:foreground ,myzen-magenta))))
   `(jabber-roster-user-chatty ((t (:foreground ,myzen-orange))))
   `(jabber-roster-user-error ((t (:foreground ,myzen-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,myzen-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,myzen-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,myzen-red+1))))
   `(jabber-chat-prompt-system ((t (:foreground ,myzen-green+3))))
   `(jabber-activity-face((t (:foreground ,myzen-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,myzen-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,myzen-orange))))
   `(js2-error ((t (:foreground ,myzen-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,myzen-green-2))))
   `(js2-jsdoc-type ((t (:foreground ,myzen-green+2))))
   `(js2-jsdoc-value ((t (:foreground ,myzen-green+3))))
   `(js2-function-param ((t (:foreground, myzen-orange))))
   `(js2-external-variable ((t (:foreground ,myzen-orange))))
;;;;; additional js2 mode attributes for better syntax highlighting
   `(js2-instance-member ((t (:foreground ,myzen-green-2))))
   `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,myzen-orange))))
   `(js2-jsdoc-html-tag-name ((t (:foreground ,myzen-red-1))))
   `(js2-object-property ((t (:foreground ,myzen-blue+1))))
   `(js2-magic-paren ((t (:foreground ,myzen-blue-5))))
   `(js2-private-function-call ((t (:foreground ,myzen-cyan))))
   `(js2-function-call ((t (:foreground ,myzen-cyan))))
   `(js2-private-member ((t (:foreground ,myzen-blue-1))))
   `(js2-keywords ((t (:foreground ,myzen-magenta))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,myzen-red-1 :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,myzen-fg :weight normal))))
   `(ledger-font-payee-pending-face ((t (:foreground ,myzen-red :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,myzen-bg+1))))
   `(ledger-font-auto-xact-face ((t (:foreground ,myzen-yellow-1 :weight normal))))
   `(ledger-font-periodic-xact-face ((t (:foreground ,myzen-green :weight normal))))
   `(ledger-font-pending-face ((t (:foreground ,myzen-orange weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,myzen-fg))))
   `(ledger-font-posting-date-face ((t (:foreground ,myzen-orange :weight normal))))
   `(ledger-font-posting-account-face ((t (:foreground ,myzen-blue-1))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,myzen-fg))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,myzen-orange))))
   `(ledger-font-posting-amount-face ((t (:foreground ,myzen-orange))))
   `(ledger-occur-narrowed-face ((t (:foreground ,myzen-fg-1 :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,myzen-bg+1))))
   `(ledger-font-comment-face ((t (:foreground ,myzen-green))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,myzen-red-1 :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,myzen-fg :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,myzen-orange :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,myzen-orange :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,myzen-green+2 :background ,myzen-bg))))
;;;;; lispy
   `(lispy-command-name-face ((t (:background ,myzen-bg-05 :inherit font-lock-function-name-face))))
   `(lispy-cursor-face ((t (:foreground ,myzen-bg :background ,myzen-fg))))
   `(lispy-face-hint ((t (:inherit highlight :foreground ,myzen-yellow))))
;;;;; ruler-mode
   `(ruler-mode-column-number ((t (:inherit 'ruler-mode-default :foreground ,myzen-fg))))
   `(ruler-mode-fill-column ((t (:inherit 'ruler-mode-default :foreground ,myzen-yellow))))
   `(ruler-mode-goal-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-comment-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-tab-stop ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-current-column ((t (:foreground ,myzen-yellow :box t))))
   `(ruler-mode-default ((t (:foreground ,myzen-green+2 :background ,myzen-bg))))

;;;;; lui
   `(lui-time-stamp-face ((t (:foreground ,myzen-blue-1))))
   `(lui-hilight-face ((t (:foreground ,myzen-green+2 :background ,myzen-bg))))
   `(lui-button-face ((t (:inherit hover-highlight))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,myzen-green+2 :background ,myzen-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,myzen-red+1 :background ,myzen-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,myzen-blue+1 :background ,myzen-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,myzen-magenta :background ,myzen-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,myzen-yellow :background ,myzen-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
;;;;;; headings and diffs
   ;; Please read (info "(magit)Theming Faces") before changing this.
   `(magit-section-highlight           ((t (:background ,myzen-bg+05))))
   `(magit-section-heading             ((t (:foreground ,myzen-yellow :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,myzen-orange :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,myzen-bg+05 :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,myzen-bg+05 :weight bold
                                                        :foreground ,myzen-orange))))
   `(magit-diff-added                  ((t (:background ,myzen-green-2))))
   `(magit-diff-added-highlight        ((t (:background ,myzen-green))))
   `(magit-diff-removed                ((t (:background ,myzen-red-4))))
   `(magit-diff-removed-highlight      ((t (:background ,myzen-red-3))))
   `(magit-diff-hunk-heading           ((t (:background ,myzen-bg+1))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,myzen-bg+2))))
   `(magit-diff-hunk-heading-selection ((t (:background ,myzen-bg+2
                                                        :foreground ,myzen-orange))))
   `(magit-diff-lines-heading          ((t (:background ,myzen-orange
                                                        :foreground ,myzen-bg+2))))
   `(magit-diff-context-highlight      ((t (:background ,myzen-bg+05
                                                        :foreground "grey70"))))
   `(magit-diffstat-added              ((t (:foreground ,myzen-green+4))))
   `(magit-diffstat-removed            ((t (:foreground ,myzen-red))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,myzen-yellow  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,myzen-green-2 :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,myzen-green   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,myzen-fg-1    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,myzen-blue-2  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,myzen-green  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,myzen-red    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground ,myzen-orange))))
   `(magit-log-date      ((t (:foreground ,myzen-fg-1))))
   `(magit-log-graph     ((t (:foreground ,myzen-fg+1))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground ,myzen-yellow-2))))
   `(magit-sequence-stop ((t (:foreground ,myzen-green))))
   `(magit-sequence-part ((t (:foreground ,myzen-yellow))))
   `(magit-sequence-head ((t (:foreground ,myzen-blue))))
   `(magit-sequence-drop ((t (:foreground ,myzen-red))))
   `(magit-sequence-done ((t (:foreground ,myzen-fg-1))))
   `(magit-sequence-onto ((t (:foreground ,myzen-fg-1))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,myzen-green))))
   `(magit-bisect-skip ((t (:foreground ,myzen-yellow))))
   `(magit-bisect-bad  ((t (:foreground ,myzen-red))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,myzen-bg-1 :foreground ,myzen-blue-2))))
   `(magit-blame-hash    ((t (:background ,myzen-bg-1 :foreground ,myzen-blue-2))))
   `(magit-blame-name    ((t (:background ,myzen-bg-1 :foreground ,myzen-orange))))
   `(magit-blame-date    ((t (:background ,myzen-bg-1 :foreground ,myzen-orange))))
   `(magit-blame-summary ((t (:background ,myzen-bg-1 :foreground ,myzen-blue-2
                                          :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,myzen-bg+3))))
   `(magit-hash           ((t (:foreground ,myzen-bg+3))))
   `(magit-tag            ((t (:foreground ,myzen-orange :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,myzen-green  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,myzen-blue   :weight bold))))
   `(magit-branch-current ((t (:foreground ,myzen-blue   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,myzen-blue   :weight bold))))
   `(magit-refname        ((t (:background ,myzen-bg+2 :foreground ,myzen-fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,myzen-bg+2 :foreground ,myzen-fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,myzen-bg+2 :foreground ,myzen-fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,myzen-green))))
   `(magit-signature-bad       ((t (:foreground ,myzen-red))))
   `(magit-signature-untrusted ((t (:foreground ,myzen-yellow))))
   `(magit-signature-expired   ((t (:foreground ,myzen-orange))))
   `(magit-signature-revoked   ((t (:foreground ,myzen-magenta))))
   '(magit-signature-error     ((t (:inherit    magit-signature-bad))))
   `(magit-cherry-unmatched    ((t (:foreground ,myzen-cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,myzen-magenta))))
   `(magit-reflog-commit       ((t (:foreground ,myzen-green))))
   `(magit-reflog-amend        ((t (:foreground ,myzen-magenta))))
   `(magit-reflog-merge        ((t (:foreground ,myzen-green))))
   `(magit-reflog-checkout     ((t (:foreground ,myzen-blue))))
   `(magit-reflog-reset        ((t (:foreground ,myzen-red))))
   `(magit-reflog-rebase       ((t (:foreground ,myzen-magenta))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,myzen-green))))
   `(magit-reflog-remote       ((t (:foreground ,myzen-cyan))))
   `(magit-reflog-other        ((t (:foreground ,myzen-cyan))))
;;;;; markup-faces
   `(markup-anchor-face ((t (:foreground ,myzen-blue+1))))
   `(markup-code-face ((t (:inherit font-lock-constant-face))))
   `(markup-command-face ((t (:foreground ,myzen-yellow))))
   `(markup-emphasis-face ((t (:inherit bold))))
   `(markup-internal-reference-face ((t (:foreground ,myzen-yellow-2 :underline t))))
   `(markup-list-face ((t (:foreground ,myzen-fg+1))))
   `(markup-meta-face ((t (:foreground ,myzen-yellow))))
   `(markup-meta-hide-face ((t (:foreground ,myzen-yellow))))
   `(markup-secondary-text-face ((t (:foreground ,myzen-yellow-1))))
   `(markup-title-0-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-1-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-2-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-3-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-4-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-typewriter-face ((t (:inherit font-lock-constant-face))))
   `(markup-verbatim-face ((t (:inherit font-lock-constant-face))))
   `(markup-value-face ((t (:foreground ,myzen-yellow))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,myzen-green+1))))
   `(message-header-other ((t (:foreground ,myzen-green))))
   `(message-header-to ((t (:foreground ,myzen-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,myzen-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,myzen-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,myzen-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,myzen-green))))
   `(message-mml ((t (:foreground ,myzen-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,myzen-orange))))
   `(mew-face-header-from ((t (:foreground ,myzen-yellow))))
   `(mew-face-header-date ((t (:foreground ,myzen-green))))
   `(mew-face-header-to ((t (:foreground ,myzen-red))))
   `(mew-face-header-key ((t (:foreground ,myzen-green))))
   `(mew-face-header-private ((t (:foreground ,myzen-green))))
   `(mew-face-header-important ((t (:foreground ,myzen-blue))))
   `(mew-face-header-marginal ((t (:foreground ,myzen-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,myzen-red))))
   `(mew-face-header-xmew ((t (:foreground ,myzen-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,myzen-red))))
   `(mew-face-body-url ((t (:foreground ,myzen-orange))))
   `(mew-face-body-comment ((t (:foreground ,myzen-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,myzen-green))))
   `(mew-face-body-cite2 ((t (:foreground ,myzen-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,myzen-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,myzen-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,myzen-red))))
   `(mew-face-mark-review ((t (:foreground ,myzen-blue))))
   `(mew-face-mark-escape ((t (:foreground ,myzen-green))))
   `(mew-face-mark-delete ((t (:foreground ,myzen-red))))
   `(mew-face-mark-unlink ((t (:foreground ,myzen-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,myzen-green))))
   `(mew-face-mark-unread ((t (:foreground ,myzen-red-2))))
   `(mew-face-eof-message ((t (:foreground ,myzen-green))))
   `(mew-face-eof-part ((t (:foreground ,myzen-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,myzen-cyan :background ,myzen-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,myzen-bg :background ,myzen-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,myzen-bg :background ,myzen-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,myzen-blue))))
   `(mingus-pausing-face ((t (:foreground ,myzen-magenta))))
   `(mingus-playing-face ((t (:foreground ,myzen-cyan))))
   `(mingus-playlist-face ((t (:foreground ,myzen-cyan ))))
   `(mingus-mark-face ((t (:bold t :foreground ,myzen-magenta))))
   `(mingus-song-file-face ((t (:foreground ,myzen-yellow))))
   `(mingus-artist-face ((t (:foreground ,myzen-cyan))))
   `(mingus-album-face ((t (:underline t :foreground ,myzen-red+1))))
   `(mingus-album-stale-face ((t (:foreground ,myzen-red+1))))
   `(mingus-stopped-face ((t (:foreground ,myzen-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,myzen-yellow))))
   `(nav-face-button-num ((t (:foreground ,myzen-cyan))))
   `(nav-face-dir ((t (:foreground ,myzen-green))))
   `(nav-face-hdir ((t (:foreground ,myzen-red))))
   `(nav-face-file ((t (:foreground ,myzen-fg))))
   `(nav-face-hfile ((t (:foreground ,myzen-red-4))))
;;;;; merlin
   `(merlin-type-face ((t (:inherit highlight))))
   `(merlin-compilation-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,myzen-orange)))
      (t
       (:underline ,myzen-orange))))
   `(merlin-compilation-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,myzen-red)))
      (t
       (:underline ,myzen-red))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,myzen-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,myzen-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,myzen-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,myzen-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,myzen-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,myzen-green-2 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,myzen-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,myzen-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,myzen-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,myzen-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,myzen-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,myzen-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,myzen-bg+1))))
;;;;; neotree
   `(neo-banner-face ((t (:foreground ,myzen-blue+1 :weight bold))))
   `(neo-header-face ((t (:foreground ,myzen-fg))))
   `(neo-root-dir-face ((t (:foreground ,myzen-blue+1 :weight bold))))
   `(neo-dir-link-face ((t (:foreground ,myzen-blue))))
   `(neo-file-link-face ((t (:foreground ,myzen-fg))))
   `(neo-expand-btn-face ((t (:foreground ,myzen-blue))))
   `(neo-vc-default-face ((t (:foreground ,myzen-fg+1))))
   `(neo-vc-user-face ((t (:foreground ,myzen-red :slant italic))))
   `(neo-vc-up-to-date-face ((t (:foreground ,myzen-fg))))
   `(neo-vc-edited-face ((t (:foreground ,myzen-magenta))))
   `(neo-vc-needs-merge-face ((t (:foreground ,myzen-red+1))))
   `(neo-vc-unlocked-changes-face ((t (:foreground ,myzen-red :background ,myzen-blue-5))))
   `(neo-vc-added-face ((t (:foreground ,myzen-green+1))))
   `(neo-vc-conflict-face ((t (:foreground ,myzen-red+1))))
   `(neo-vc-missing-face ((t (:foreground ,myzen-red+1))))
   `(neo-vc-ignored-face ((t (:foreground ,myzen-fg-1))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,myzen-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,myzen-fg :weight bold))))
   `(org-block ((t (:background ,myzen-bg+05 :extend t))))
   `(org-checkbox ((t (:background ,myzen-bg+2 :foreground ,myzen-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,myzen-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,myzen-red-1))))
   `(org-done ((t (:weight bold :weight bold :foreground ,myzen-green+3))))
   `(org-formula ((t (:foreground ,myzen-yellow-2))))
   `(org-headline-done ((t (:foreground ,myzen-green+3))))
   `(org-hide ((t (:foreground ,myzen-bg))))
   `(org-level-1 ((t (:inherit ,z-variable-pitch :foreground ,myzen-orange
                               ,@(when myzen-scale-org-headlines
                                   (list :height myzen-height-plus-4))))))
   `(org-level-2 ((t (:inherit ,z-variable-pitch :foreground ,myzen-green+4
                               ,@(when myzen-scale-org-headlines
                                   (list :height myzen-height-plus-3))))))
   `(org-level-3 ((t (:inherit ,z-variable-pitch :foreground ,myzen-blue-1
                               ,@(when myzen-scale-org-headlines
                                   (list :height myzen-height-plus-2))))))
   `(org-level-4 ((t (:inherit ,z-variable-pitch :foreground ,myzen-yellow-2
                               ,@(when myzen-scale-org-headlines
                                   (list :height myzen-height-plus-1))))))
   `(org-level-5 ((t (:inherit ,z-variable-pitch :foreground ,myzen-cyan))))
   `(org-level-6 ((t (:inherit ,z-variable-pitch :foreground ,myzen-green+2))))
   `(org-level-7 ((t (:inherit ,z-variable-pitch :foreground ,myzen-red-4))))
   `(org-level-8 ((t (:inherit ,z-variable-pitch :foreground ,myzen-blue-4))))
   `(org-link ((t (:foreground ,myzen-yellow-2 :underline t))))
   `(org-quote ((t (:background ,myzen-bg+05 :extend t))))
   `(org-scheduled ((t (:foreground ,myzen-green+4))))
   `(org-scheduled-previously ((t (:foreground ,myzen-red))))
   `(org-scheduled-today ((t (:foreground ,myzen-blue+1))))
   `(org-sexp-date ((t (:foreground ,myzen-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,myzen-green+2))))
   `(org-tag ((t (:weight bold :weight bold))))
   `(org-time-grid ((t (:foreground ,myzen-orange))))
   `(org-todo ((t (:weight bold :foreground ,myzen-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:weight bold :foreground ,myzen-red :weight bold :underline nil))))
   `(org-column ((t (:background ,myzen-bg-1))))
   `(org-column-title ((t (:background ,myzen-bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,myzen-fg :background ,myzen-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,myzen-bg :background ,myzen-red-1))))
   `(org-ellipsis ((t (:foreground ,myzen-yellow-1 :underline t))))
   `(org-footnote ((t (:foreground ,myzen-cyan :underline t))))
   `(org-document-title ((t (:inherit ,z-variable-pitch :foreground ,myzen-blue
                                      :weight bold
                                      ,@(when myzen-scale-org-headlines
                                          (list :height myzen-height-plus-4))))))
   `(org-document-info ((t (:foreground ,myzen-blue))))
   `(org-habit-ready-face ((t :background ,myzen-green)))
   `(org-habit-alert-face ((t :background ,myzen-yellow-1 :foreground ,myzen-bg)))
   `(org-habit-clear-face ((t :background ,myzen-blue-3)))
   `(org-habit-overdue-face ((t :background ,myzen-red-3)))
   `(org-habit-clear-future-face ((t :background ,myzen-blue-4)))
   `(org-habit-ready-future-face ((t :background ,myzen-green-2)))
   `(org-habit-alert-future-face ((t :background ,myzen-yellow-2 :foreground ,myzen-bg)))
   `(org-habit-overdue-future-face ((t :background ,myzen-red-4)))
;;;;; org-ref
   `(org-ref-ref-face ((t :underline t)))
   `(org-ref-label-face ((t :underline t)))
   `(org-ref-cite-face ((t :underline t)))
   `(org-ref-glossary-face ((t :underline t)))
   `(org-ref-acronym-face ((t :underline t)))
;;;;; outline
   `(outline-1 ((t (:inherit ,z-variable-pitch :foreground ,myzen-orange
                             ,@(when myzen-scale-outline-headlines
                                 (list :height myzen-height-plus-4))))))
   `(outline-2 ((t (:inherit ,z-variable-pitch :foreground ,myzen-green+4
                             ,@(when myzen-scale-outline-headlines
                                 (list :height myzen-height-plus-3))))))
   `(outline-3 ((t (:inherit ,z-variable-pitch :foreground ,myzen-blue-1
                             ,@(when myzen-scale-outline-headlines
                                 (list :height myzen-height-plus-2))))))
   `(outline-4 ((t (:inherit ,z-variable-pitch :foreground ,myzen-yellow-2
                             ,@(when myzen-scale-outline-headlines
                                 (list :height myzen-height-plus-1))))))
   `(outline-5 ((t (:inherit ,z-variable-pitch :foreground ,myzen-cyan))))
   `(outline-6 ((t (:inherit ,z-variable-pitch :foreground ,myzen-green+2))))
   `(outline-7 ((t (:inherit ,z-variable-pitch :foreground ,myzen-red-4))))
   `(outline-8 ((t (:inherit ,z-variable-pitch :foreground ,myzen-blue-4))))
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; c/perl
   `(cperl-nonoverridable-face ((t (:foreground ,myzen-magenta))))
   `(cperl-array-face ((t (:foreground ,myzen-yellow, :backgorund ,myzen-bg))))
   `(cperl-hash-face ((t (:foreground ,myzen-yellow-1, :background ,myzen-bg))))
;;;;; paren-face
   `(parenthesis ((t (:foreground ,myzen-fg-1))))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,myzen-yellow-2))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,myzen-bg-05 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,myzen-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,myzen-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,myzen-bg+3 :inherit mode-line-inactive))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,myzen-fg :background ,myzen-bg+2))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,myzen-bg :background ,myzen-orange))))
   `(proof-error-face ((t (:foreground ,myzen-fg :background ,myzen-red-4))))
   `(proof-highlight-dependency-face ((t (:foreground ,myzen-bg :background ,myzen-yellow-1))))
   `(proof-highlight-dependent-face ((t (:foreground ,myzen-bg :background ,myzen-orange))))
   `(proof-locked-face ((t (:background ,myzen-blue-5))))
   `(proof-mouse-highlight-face ((t (:foreground ,myzen-bg :background ,myzen-orange))))
   `(proof-queue-face ((t (:background ,myzen-red-4))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,myzen-red-2))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,myzen-bg))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,myzen-bg))))
   `(proof-warning-face ((t (:foreground ,myzen-bg :background ,myzen-yellow-1))))
;;;;; racket-mode
   `(racket-keyword-argument-face ((t (:inherit font-lock-constant-face))))
   `(racket-selfeval-face ((t (:inherit font-lock-type-face))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,myzen-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,myzen-green+4))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,myzen-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,myzen-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,myzen-green+2))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,myzen-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,myzen-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,myzen-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,myzen-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,myzen-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,myzen-green))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,myzen-blue-5))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,myzen-blue))))
   `(rcirc-other-nick ((t (:foreground ,myzen-orange))))
   `(rcirc-bright-nick ((t (:foreground ,myzen-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,myzen-blue-2))))
   `(rcirc-server ((t (:foreground ,myzen-green))))
   `(rcirc-server-prefix ((t (:foreground ,myzen-green+1))))
   `(rcirc-timestamp ((t (:foreground ,myzen-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,myzen-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:weight bold))))
   `(rcirc-prompt ((t (:foreground ,myzen-yellow :weight bold))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:weight bold))))
   `(rcirc-url ((t (:weight bold))))
   `(rcirc-keyword ((t (:foreground ,myzen-yellow :weight bold))))
;;;;; re-builder
   `(reb-match-0 ((t (:foreground ,myzen-bg :background ,myzen-magenta))))
   `(reb-match-1 ((t (:foreground ,myzen-bg :background ,myzen-blue))))
   `(reb-match-2 ((t (:foreground ,myzen-bg :background ,myzen-orange))))
   `(reb-match-3 ((t (:foreground ,myzen-bg :background ,myzen-red))))
;;;;; realgud
   `(realgud-overlay-arrow1 ((t (:foreground ,myzen-green))))
   `(realgud-overlay-arrow2 ((t (:foreground ,myzen-yellow))))
   `(realgud-overlay-arrow3 ((t (:foreground ,myzen-orange))))
   `(realgud-bp-enabled-face ((t (:inherit error))))
   `(realgud-bp-disabled-face ((t (:inherit secondary-selection))))
   `(realgud-bp-line-enabled-face ((t (:box (:color ,myzen-red :style nil)))))
   `(realgud-bp-line-disabled-face ((t (:box (:color "grey70" :style nil)))))
   `(realgud-line-number ((t (:foreground ,myzen-yellow))))
   `(realgud-backtrace-number ((t (:foreground ,myzen-yellow, :weight bold))))
;;;;; regex-tool
   `(regex-tool-matched-face ((t (:background ,myzen-blue-4 :weight bold))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,myzen-green))))
   `(rpm-spec-doc-face ((t (:foreground ,myzen-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,myzen-red))))
   `(rpm-spec-macro-face ((t (:foreground ,myzen-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,myzen-red))))
   `(rpm-spec-package-face ((t (:foreground ,myzen-red))))
   `(rpm-spec-section-face ((t (:foreground ,myzen-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,myzen-blue))))
   `(rpm-spec-var-face ((t (:foreground ,myzen-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,myzen-orange))))
   `(rst-level-2-face ((t (:foreground ,myzen-green+1))))
   `(rst-level-3-face ((t (:foreground ,myzen-blue-1))))
   `(rst-level-4-face ((t (:foreground ,myzen-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,myzen-cyan))))
   `(rst-level-6-face ((t (:foreground ,myzen-green-2))))
;;;;; selectrum
   `(selectrum-current-candidate ((t (:foreground ,myzen-yellow :weight bold :underline t))))
   `(selectrum-primary-highlight ((t (:background ,myzen-green-2))))
   `(selectrum-secondary-highlight ((t (:background ,myzen-green))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,myzen-yellow :weight bold))))
   `(sh-quoted-exec ((t (:foreground ,myzen-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,myzen-red+1 :background ,myzen-bg+3 :weight bold))))
   `(show-paren-match ((t (:foreground ,myzen-fg :background ,myzen-bg+3 :weight bold))))
;;;;; smart-mode-line
   ;; use (setq sml/theme nil) to enable myzen for sml
   `(sml/global ((,class (:foreground ,myzen-fg :weight bold))))
   `(sml/modes ((,class (:foreground ,myzen-yellow :weight bold))))
   `(sml/minor-modes ((,class (:foreground ,myzen-fg-1 :weight bold))))
   `(sml/filename ((,class (:foreground ,myzen-yellow :weight bold))))
   `(sml/line-number ((,class (:foreground ,myzen-blue :weight bold))))
   `(sml/col-number ((,class (:foreground ,myzen-blue+1 :weight bold))))
   `(sml/position-percentage ((,class (:foreground ,myzen-blue-1 :weight bold))))
   `(sml/prefix ((,class (:foreground ,myzen-orange))))
   `(sml/git ((,class (:foreground ,myzen-green+3))))
   `(sml/process ((,class (:weight bold))))
   `(sml/sudo ((,class  (:foreground ,myzen-orange :weight bold))))
   `(sml/read-only ((,class (:foreground ,myzen-red-2))))
   `(sml/outside-modified ((,class (:foreground ,myzen-orange))))
   `(sml/modified ((,class (:foreground ,myzen-red))))
   `(sml/vc-edited ((,class (:foreground ,myzen-green+2))))
   `(sml/charging ((,class (:foreground ,myzen-green+4))))
   `(sml/discharging ((,class (:foreground ,myzen-red+1))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,myzen-red+1 :background ,myzen-bg+3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,myzen-bg+3 :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,myzen-red))))
   `(slime-repl-inputed-output-face ((t (:foreground ,myzen-green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,myzen-red)))
      (t
       (:underline ,myzen-red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,myzen-orange)))
      (t
       (:underline ,myzen-orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,myzen-yellow)))
      (t
       (:underline ,myzen-yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,myzen-green)))
      (t
       (:underline ,myzen-green))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; solaire
   `(solaire-default-face ((t (:inherit default :background ,myzen-bg-08))))
   `(solaire-minibuffer-face ((t (:inherit default :background ,myzen-bg-08))))
   `(solaire-hl-line-face ((t (:inherit hl-line :background ,myzen-bg))))
   `(solaire-org-hide-face ((t (:inherit org-hide :background ,myzen-bg-08))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,myzen-green+2))))
   `(speedbar-directory-face ((t (:foreground ,myzen-cyan))))
   `(speedbar-file-face ((t (:foreground ,myzen-fg))))
   `(speedbar-highlight-face ((t (:foreground ,myzen-bg :background ,myzen-green+2))))
   `(speedbar-selected-face ((t (:foreground ,myzen-red))))
   `(speedbar-separator-face ((t (:foreground ,myzen-bg :background ,myzen-blue-1))))
   `(speedbar-tag-face ((t (:foreground ,myzen-yellow))))
;;;;; swiper
   `(swiper-line-face ((t (:underline t))))
;;;;; sx
   `(sx-custom-button
     ((t (:background ,myzen-fg :foreground ,myzen-bg-1
          :box (:line-width 3 :style released-button) :height 0.9))))
   `(sx-question-list-answers
     ((t (:foreground ,myzen-green+3
          :height 1.0 :inherit sx-question-list-parent))))
   `(sx-question-mode-accepted
     ((t (:foreground ,myzen-green+3
          :height 1.3 :inherit sx-question-mode-title))))
   '(sx-question-mode-content-face ((t (:inherit highlight))))
   `(sx-question-mode-kbd-tag
     ((t (:box (:color ,myzen-bg-1 :line-width 3 :style released-button)
          :height 0.9 :weight semi-bold))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,myzen-fg
                                    :background ,myzen-bg))))
   `(tabbar-selected ((t (:foreground ,myzen-fg
                                      :background ,myzen-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,myzen-fg
                                        :background ,myzen-bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,myzen-bg
                                       :background ,myzen-bg-1))))
   `(term-color-red ((t (:foreground ,myzen-red-2
                                     :background ,myzen-red-4))))
   `(term-color-green ((t (:foreground ,myzen-green
                                       :background ,myzen-green+2))))
   `(term-color-yellow ((t (:foreground ,myzen-orange
                                        :background ,myzen-yellow))))
   `(term-color-blue ((t (:foreground ,myzen-blue-1
                                      :background ,myzen-blue-4))))
   `(term-color-magenta ((t (:foreground ,myzen-magenta
                                         :background ,myzen-red))))
   `(term-color-cyan ((t (:foreground ,myzen-cyan
                                      :background ,myzen-blue))))
   `(term-color-white ((t (:foreground ,myzen-fg
                                       :background ,myzen-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,myzen-fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,myzen-red-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,myzen-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,myzen-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,myzen-cyan))))
;;;;; visual-regexp
   `(vr/group-0 ((t (:foreground ,myzen-bg :background ,myzen-green :weight bold))))
   `(vr/group-1 ((t (:foreground ,myzen-bg :background ,myzen-orange :weight bold))))
   `(vr/group-2 ((t (:foreground ,myzen-bg :background ,myzen-blue :weight bold))))
   `(vr/match-0 ((t (:inherit isearch))))
   `(vr/match-1 ((t (:foreground ,myzen-yellow-2 :background ,myzen-bg-1 :weight bold))))
   `(vr/match-separator-face ((t (:foreground ,myzen-red :weight bold))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,myzen-bg-05))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,myzen-orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,myzen-orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,myzen-green+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,myzen-blue))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,myzen-blue))))
   `(web-mode-html-attr-name-face ((t (:foreground ,myzen-orange))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,myzen-cyan))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,myzen-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,myzen-red))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,myzen-bg+1 :foreground ,myzen-bg+1))))
   `(whitespace-hspace ((t (:background ,myzen-bg+1 :foreground ,myzen-bg+1))))
   `(whitespace-tab ((t (:background ,myzen-red-1))))
   `(whitespace-newline ((t (:foreground ,myzen-bg+1))))
   `(whitespace-trailing ((t (:background ,myzen-red))))
   `(whitespace-line ((t (:background ,myzen-bg :foreground ,myzen-magenta))))
   `(whitespace-space-before-tab ((t (:background ,myzen-orange :foreground ,myzen-orange))))
   `(whitespace-indentation ((t (:background ,myzen-yellow :foreground ,myzen-red))))
   `(whitespace-empty ((t (:background ,myzen-yellow))))
   `(whitespace-space-after-tab ((t (:background ,myzen-yellow :foreground ,myzen-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,myzen-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,myzen-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,myzen-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,myzen-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,myzen-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,myzen-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,myzen-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,myzen-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,myzen-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,myzen-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,myzen-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,myzen-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,myzen-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,myzen-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,myzen-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,myzen-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,myzen-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,myzen-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,myzen-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,myzen-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,myzen-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,myzen-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,myzen-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,myzen-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,myzen-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,myzen-green+4))))
;;;;; xcscope
   `(cscope-file-face ((t (:foreground ,myzen-yellow :weight bold))))
   `(cscope-function-face ((t (:foreground ,myzen-cyan :weight bold))))
   `(cscope-line-number-face ((t (:foreground ,myzen-red :weight bold))))
   `(cscope-mouse-face ((t (:foreground ,myzen-bg :background ,myzen-blue+1))))
   `(cscope-separator-face ((t (:foreground ,myzen-red :weight bold
                                            :underline t :overline t))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,myzen-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,myzen-bg-1 :foreground ,myzen-bg-1))))
   ))

;;; Theme Variables
(myzen-with-color-variables
  (custom-theme-set-variables
   'myzen
;;;;; ansi-color
   `(ansi-color-names-vector [,myzen-bg ,myzen-red ,myzen-green ,myzen-yellow
                                          ,myzen-blue ,myzen-magenta ,myzen-cyan ,myzen-fg])
;;;;; company-quickhelp
   `(company-quickhelp-color-background ,myzen-bg+1)
   `(company-quickhelp-color-foreground ,myzen-fg)
;;;;; fill-column-indicator
   `(fci-rule-color ,myzen-bg-05)
;;;;; nrepl-client
   `(nrepl-message-colors
     '(,myzen-red ,myzen-orange ,myzen-yellow ,myzen-green ,myzen-green+4
       ,myzen-cyan ,myzen-blue+1 ,myzen-magenta))
;;;;; pdf-tools
   `(pdf-view-midnight-colors '(,myzen-fg . ,myzen-bg-05))
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,myzen-red-1)
       ( 40. . ,myzen-red)
       ( 60. . ,myzen-orange)
       ( 80. . ,myzen-yellow-2)
       (100. . ,myzen-yellow-1)
       (120. . ,myzen-yellow)
       (140. . ,myzen-green-2)
       (160. . ,myzen-green)
       (180. . ,myzen-green+1)
       (200. . ,myzen-green+2)
       (220. . ,myzen-green+3)
       (240. . ,myzen-green+4)
       (260. . ,myzen-cyan)
       (280. . ,myzen-blue-2)
       (300. . ,myzen-blue-1)
       (320. . ,myzen-blue)
       (340. . ,myzen-blue+1)
       (360. . ,myzen-magenta)))
   `(vc-annotate-very-old-color ,myzen-magenta)
   `(vc-annotate-background ,myzen-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defcustom myzen-add-font-lock-keywords nil
  "Whether to add font-lock keywords for myzen color names.

In buffers visiting library `myzen-theme.el' the myzen
specific keywords are always added, provided that library has
been loaded (because that is where the code that does it is
defined).  If you visit this file and only enable the theme,
then you have to turn `rainbow-mode' off and on again for the
myzen-specific font-lock keywords to be used.

In all other Emacs-Lisp buffers this variable controls whether
this should be done.  This requires library `rainbow-mode'."
  :type 'boolean
  :group 'myzen-theme)

(defvar myzen-colors-font-lock-keywords nil)

(defun myzen--rainbow-turn-on ()
  "Maybe also add font-lock keywords for myzen colors."
  (when (and (derived-mode-p 'emacs-lisp-mode)
             (or myzen-add-font-lock-keywords
                 (and (buffer-file-name)
                      (equal (file-name-nondirectory (buffer-file-name))
                             "myzen-theme.el"))))
    (unless myzen-colors-font-lock-keywords
      (setq myzen-colors-font-lock-keywords
            `((,(regexp-opt (mapcar 'car myzen-default-colors-alist) 'words)
               (0 (rainbow-colorize-by-assoc myzen-default-colors-alist))))))
    (font-lock-add-keywords nil myzen-colors-font-lock-keywords 'end)))

(defun myzen--rainbow-turn-off ()
  "Also remove font-lock keywords for myzen colors."
  (font-lock-remove-keywords nil myzen-colors-font-lock-keywords))

(when (fboundp 'advice-add)
  (advice-add 'rainbow-turn-on :after  #'myzen--rainbow-turn-on)
  (advice-add 'rainbow-turn-off :after #'myzen--rainbow-turn-off))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'myzen)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; myzen-theme.el ends here
