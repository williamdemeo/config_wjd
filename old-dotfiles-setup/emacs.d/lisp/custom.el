; FILE: wjd.el  (previous names: custom_wjd_init.el)
; BLAME: <williamdemeo@gmail.com>
; DESC: personal customizations for emacs
; DATE: 1 Jan 2011

; UPDATES (see dotfiles_wjd git repo for details)
;  23 Mar 2020 <williamdemeo@gmail.com>
;  23 Jul 2013 <williamdemeo@gmail.com>
;  01 Jan 2011 <williamdemeo@gmail.com>

(message "Loading custom.el")


(scroll-bar-mode -1) ; ...because what self-respecting emacs user wants a scrollbar?


;--geometry------------------------------------------------------------------------
(defun arrange-frame (w h x y)
  "Set the width, height, and x/y position of the current frame"
  (let ((frame (selected-frame)))
    (delete-other-windows)
    (set-frame-position frame x y)
    (set-frame-size frame w h)))

(when (display-graphic-p)
  (arrange-frame 139 76 0 0)  ; <<<< set the w h x y variables here
)

;--packages----------------------------------------------------------------
(require 'package)
(setq package-archives
       '(
; >>>> These take way too long to load <<<<
      ("melpa-stable" . "http://stable.melpa.org/packages/")
 	("melpa"     . "http://melpa.milkbox.net/packages/")
; >>>>                                 <<<<
;	("marmalade" . "http://marmalade-repo.org/packages/")
       ("gnu"       . "http://elpa.gnu.org/packages/")))
(package-initialize)

;-- Magit ------------------------------------------------------------
 ; Install magit, if you haven't already done so, as follows:
 ; `M-x package-refresh-contents` and `M-x package-install [Enter] magit`
 ; (define-key global-map "\M-gm" 'magit-status)

;-- Haskell ------------------------------------------------------------
; (require 'package)
; (package-initialize)
; (package-refresh-contents)
; (package-install 'intero) ; comes with ghc-mod, flycheck, company
; (add-hook 'haskell-mode-hook 'intero-mode)

 ;; Tidal cycles
 ;(add-to-list 'load-path "~/git/TEAMS/TypeFunc/gh/uh-mfc/haskelltalk/") ;; Look for tidal.el in ~/uh-mfc/haskelltalk/
 ;(require 'tidal)
 ;(setq tidal-interpreter "~/git/TEAMS/TypeFunc/gh/uh-mfc/haskelltalk/ghciscript")
;------------------------------


;;;;--------- For Scala -------------------
;(unless (package-installed-p 'scala-mode2)
;  (package-refresh-contents) (package-install 'scala-mode2))
;;;;----------------------------------------------

;;; uncomment this line to disable loading of "default.el" at startup
(setq inhibit-default-init t)


;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; enable visual feedback on selections
;(setq transient-mark-mode t)

;; The rest was mainly taken from my old init.el file
;; For other features left out of this version, see
;; the file old_custom_init.el


;;
;; Global key bindings
(define-key global-map "\C-cc" 'compile)
(define-key global-map "\C-cs" 'shell)
(define-key global-map "\C-cg" 'gdb)


;; (autoload 'noweb-mode "noweb-mode" "noweb mode." t)
;; (setq auto-mode-alist
;;       (cons '("\\.nw$" . noweb-mode) auto-mode-alist))

;; (add-hook 'noweb-mode-hook
;; 	  '(lambda ()
;; 	     (setq-default noweb-set-code-mode "matlab-mode")
;; 	     (setq-default noweb-set-doc-mode "LaTeX-mode")))

;;
;;  LaTeX Mode
;;
(setq auto-mode-alist
      (cons '("\\.tex$" . LaTeX-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.def$" . LaTeX-mode) auto-mode-alist))
(add-hook 'LaTeX-mode-hook
          (lambda ()
;	    (auto-fill-mode 1)
	    (font-lock-mode 1)))
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode



(setq reftex-external-file-finders
      '(("tex" . "kpsewhich -format=.tex %f")
       ("bib" . "kpsewhich -format=.bib %f")))


 (add-hook 'latex-mode-hook
 	  (lambda ()
; 	    (auto-fill-mode t)
	    (reftex-mode t)))


;; >>>> Set the default location for your main .bib database here <<<<
(setq reftex-default-bibliography '("~/Dropbox/RESEARCH/wjd.bib"))

;;
;;  BibTex Mode
;;
(setq auto-mode-alist
      (cons '("\\.bib$" . bibtex-mode) auto-mode-alist))
(add-hook 'bibtex-mode-hook 'turn-on-font-lock)

;;
;;  Markdown Mode
;;
(setq auto-mode-alist (append (list '("\\.md$" . markdown-mode)
                                    '("\\.markdown$" . markdown-mode))
                              auto-mode-alist))
(add-hook 'markdown-mode-hook (lambda ()
;				(auto-fill-mode 1)
				))

;;
;; NOTES
;;
;[Changing colours]
;  First, find the font name to change using the command:
;    M-x list-text-properties-at
;  Then, suppose you got `font-latex-math-face', edit ~/.Xdefaults and add:
;    Emacs.font-latex-math-face.attributeForeground: blue

;;
;; From file:///usr/share/doc/xemacs21/README.Debian
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(case-replace nil)
 '(text-scale-mode-step 1.05)
 '(column-number-mode t)
 '(fill-column 80)
 '(font-lock-maximum-decoration t)
 '(fringe-mode 1 nil (fringe))
 '(gutter-buffers-tab-visible-p nil)
 '(paren-mode (quote paren) nil (paren))
 '(scrollbars-visible-p nil)
 '(tool-bar-mode nil)
 '(toolbar-visible-p nil)
 '(unshifted-motion-keys-deselect-region nil)
 '(package-selected-packages
   (quote
    (latex-pretty-symbols zenburn-theme zenburn unicode-math-input unicode-input unicode-fonts unicode-escape org-journal markdown-mode latex-unicode-math-mode intero heroku-theme hc-zenburn-theme flatland-theme company-math)))
 '(smartparens-mode nil)
                                        ; '(paren-mode (quote paren) nil (paren))
 '(electric-indent-mode nil)
 

)

;; Gap

      ;; (autoload 'gap-mode "gap-mode" "Gap editing mode" t)
      ;; (setq auto-mode-alist (apply 'list
      ;;                              '("\\.g$" . gap-mode)
      ;;                              '("\\.gap$" . gap-mode)
      ;;                              auto-mode-alist))
;; org-mode
;; (require 'package)
;; ;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;; (require 'org-journal)
;; (custom-set-variables
;;  '(org-journal-dir "~/git/lab/williamdemeo/org-projects/journal"))


;; Agda, etc.
;(load "~/.emacs.d/lisp/itp.el")

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ansi-color-names-vector
;;    ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
;;  '(case-replace nil)
;;  '(column-number-mode t)
;;  '(fill-column 80)
;;  '(font-lock-maximum-decoration t)
;;  '(fringe-mode 1 nil (fringe))
;;  '(gutter-buffers-tab-visible-p nil)
;;  '(org-journal-dir "~/git/lab/williamdemeo/org-projects/journal")
;;  '(package-selected-packages
;;    (quote
;;     (latex-pretty-symbols zenburn-theme zenburn unicode-math-input unicode-input unicode-fonts unicode-escape org-journal markdown-mode magit latex-unicode-math-mode intero heroku-theme hc-zenburn-theme flatland-theme company-math)))
;;  '(paren-mode (quote paren) nil (paren))
;;  '(scrollbars-visible-p nil)
;;  '(tool-bar-mode nil)
;;  '(toolbar-visible-p nil)
;;  '(unshifted-motion-keys-deselect-region nil))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )

;;;; ---Agda customizations----------------
(setq load-path (cons "~/git/lab/wjd/UF-Agda_wjd/agda/.cabal-sandbox/bin" load-path))
;; (set-fontset-font "fontset-default" nil
;;                   (font-spec :name "DejaVu Sans"))

(setenv "LD_LIBRARY_PATH"
  (let ((current (getenv "LD_LIBRARY_PATH"))
        (new "/usr/local/lib:~/git/lab/wjd/UF-Agda_wjd/agda/.cabal-sandbox/bin"))
    (if current (concat new ":" current) new)))

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

(setq auto-mode-alist
   (append
     '(("\\.agda\\'" . agda2-mode)
       ("\\.lagda.md\\'" . agda2-mode))
     auto-mode-alist))

;; '(agda2-include-dirs (quote ("." "/home/williamdemeo/git/PROGRAMMING/AGDA/agda-stdlib/src")))

(add-hook 'agda2-mode-hook (lambda ()
;       (require 'indent-hint)
       (setq smartparens-mode nil)
       (setq electric-indent-mode nil) ) )

