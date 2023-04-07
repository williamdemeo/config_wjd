;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
(setq smartparens-mode nil)  ;; I can't stand smartparens!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "William DeMeo"
      user-mail-address "williamdemeo@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(add-to-list 'load-path "~/.local/straight/build-28.2/unicode-fonts")
;;(require 'unicode-fonts)
;;(unicode-fonts-setup)
;;(require 'font-utils)
;;(font-utils-exists-p "Courier")

;;(set-fontset-font "fontset-default" nil
;;                (font-spec :name "DejaVu Sans"))


;;(setq doom-unicode-font (font-spec :family "Fira Mono"))


;(setq
;  doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;  doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13)
;  doom-unicode-font (font-spec :family "Fira Mono")
;)
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
; (setq doom-theme 'doom-dark+)
(setq doom-theme 'doom-zenburn)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/git/williamdemeo/org-project")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

(add-hook 'agda2-mode-hook (lambda ()
                             (setq smartparens-mode nil)
                             (setq electric-indent-mode nil)
                              C-c l f         agda2-next-goal

                             ))

(setq my/ledger-agda-name "~/IOHK/ledger-agda")
(defun my/toggle-ledger-agda ()
  (interactive)
  (if (string-equal agda2-program-name "agda")
      (setq agda2-program-name (concat my/ledger-agda-name "/bin/agda"))
    (setq agda2-program-name "agda"))
  (agda2-restart))
(with-eval-after-load 'agda2-mode (define-key agda2-mode-map (kbd "C-c C-x C-t") 'my/toggle-ledger-agda))



;; git blamer mode
;;
(use-package blamer
  :bind (("s-i" . blamer-show-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  ;; :custom-face
  ;; (blamer-face ((t :foreground "#7a88cf"
  ;;                   :background nil
  ;;                   :height 140
  ;;                   :italic t)))
  :config
  (global-blamer-mode 1))
