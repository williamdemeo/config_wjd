; FILE: init.el
; BLAME: <williamdemeo@gmail.com>
; DESC: initialization file for emacs
;
; UPDATES (see dotfiles_wjd git repo for details)
;  23 Jul 2013 <williamdemeo@gmail.com>
;  29 Nov 2011 <williamdemeo@gmail.com>
;  26 Apr 2009 <williamdemeo@gmail.com>
;  09 Jan 2004 by <williamdemeo@yahoo.com>
;
(message "Loading init.el")

;; Added by Package.el.  This must come before configurations of installed packages.
(package-initialize)


;(setq load-path (cons "~/.emacs.d/lisp" load-path))
(setq load-path (cons "~/git/lab/wjd/utils/dotfiles-setup/emacs.d/lisp" load-path))
; (setq load-path (cons (file-name-directory load-file-name) load-path))

(load "custom.el")

;--iteractive theorem provers----------------------------------------------------------
(load "itp.el")

;--spacemacs-------------------------------------------------------------------------
(setq spacemacs-start-directory "~/.emacs.d/spacemacs/")
(load-file (concat spacemacs-start-directory "init.el"))

