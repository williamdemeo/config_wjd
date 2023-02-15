;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" system-name))

(setq load-path (cons "~/.elisp" load-path))
(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
;;(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)
 (add-hook 'matlab-mode-hook
 	  (lambda () 
 	    (auto-fill-mode nil)))

 (add-hook 'latex-mode-hook
 	  (lambda () 
 	    (auto-fill-mode t)
	    (reftex-mode t)))


;; (setq default-frame-alist
;;       '(
;;         (cursor-color . "orange")
;;         (cursor-type . box)
;;         (foreground-color . "black")
;;         (background-color . "DarkSlateGray")
;;         (menu-bar-lines . 0)
;;         (width . 187)
;;         (height . 48)
;;         )
;;       )

;for auctex
;(require 'tex-site)

;(add-to-list 'load-path "~/pub/share/wiki")
;(add-to-list 'load-path "~/pub/share/planner")
;(require 'planner)
;(plan)

;;;; THE FOLLOWING SHOULD ALL GO IN init.el!!!

;; (custom-set-variables
;;    ;; custom-set-variables was added by Custom.
;;    ;; If you edit it by hand, you could mess it up, so be careful.
;;    ;; Your init file should contain only one such instance.
;;    ;; If there is more than one, they won't work right.
;;   '(case-replace nil)
;;   '(tool-bar-mode nil)
;;   '(toolbar-visible-p nil)
;;   '(column-number-mode t)
;;   '(auto-fill-mode t)
;;   '(fill-column 80)
;;   '(font-lock-maximum-decoration t)
;;   '(gutter-buffers-tab-visible-p nil)
;;   '(line-number-mode t)
;; ;;  '(matlab-verify-on-save-flag nil)
;;   '(paren-mode (quote paren) nil (paren))
;;   '(scrollbars-visible-p nil)
;;   '(unshifted-motion-keys-deselect-region nil)
;; )

;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))


;; ; (custom-set-faces
;; ; '(default ((t (:size "14pt" :family "Andale Mono"))) t))


;; ;; default-major-mode is used if no match in auto-mode-alist 
;; ;(setq default-major-mode 'text-mode)

;; ;; kill the tool bar
;; ;;(tool-bar-mode nil)
      (autoload 'gap-mode "gap-mode" "Gap editing mode" t)
      (setq auto-mode-alist (apply 'list
                                   '("\\.g$" . gap-mode)
                                   '("\\.gap$" . gap-mode)
                                   auto-mode-alist))
