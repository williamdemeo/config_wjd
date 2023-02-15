;;; init.el --- Spacemacs Initialization File -*- no-byte-compile: t -*-
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

;; Avoid garbage collection during startup.
;; see `SPC h . dotspacemacs-gc-cons' for more info
(defconst emacs-start-time (current-time))
(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)
(load (concat (file-name-directory load-file-name) "core/core-load-paths")
      nil (not init-file-debug))
(load (concat spacemacs-core-directory "core-versions")
      nil (not init-file-debug))
(load (concat spacemacs-core-directory "core-dumper")
      nil (not init-file-debug))

;; Remove compiled core files if they become stale or Emacs version has changed.
(load (concat spacemacs-core-directory "core-compilation")
      nil (not init-file-debug))
(load spacemacs--last-emacs-version-file t (not init-file-debug))
(when (or (not (string= spacemacs--last-emacs-version emacs-version))
          (> 0 (spacemacs//dir-byte-compile-state
                (concat spacemacs-core-directory "libs/"))))
  (spacemacs//remove-byte-compiled-files-in-dir spacemacs-core-directory))
;; Update saved Emacs version.
(unless (string= spacemacs--last-emacs-version emacs-version)
  (spacemacs//update-last-emacs-version))

(if (not (version<= spacemacs-emacs-min-version emacs-version))
    (error (concat "Your version of Emacs (%s) is too old. "
                   "Spacemacs requires Emacs version %s or above.")
           emacs-version spacemacs-emacs-min-version)
  ;; Disabling file-name-handlers for a speed boost during init might seem like
  ;; a good idea but it causes issues like
  ;; https://github.com/syl20bnr/spacemacs/issues/11585 "Symbol's value as
  ;; variable is void: \213" when emacs is not built having:
  ;; `--without-compress-install`
  (let ((please-do-not-disable-file-name-handler-alist nil))
    (require 'core-spacemacs)
    (spacemacs/dump-restore-load-path)
    (configuration-layer/load-lock-file)
    (spacemacs/init)
    (configuration-layer/stable-elpa-init)
    (configuration-layer/load)
    (spacemacs-buffer/display-startup-note)
    (spacemacs/setup-startup-hook)
    (spacemacs/dump-eval-delayed-functions)
    (when (and dotspacemacs-enable-server (not (spacemacs-is-dumping-p)))
      (require 'server)
      (when dotspacemacs-server-socket-dir
        (setq server-socket-dir dotspacemacs-server-socket-dir))
      (unless (server-running-p)
        (message "Starting a server...")
        (server-start)))))


;; AGDA-config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TODO: Fix this so we can toggle between then instead of using comments.
;; For now, there are two lines, marked with ***, that can be adjusted by hand to switch
;; between alternative Agda setups.
;;
;; *** Edit this `setq load-path` line --------
;;   For agda-algebras:
     (setq load-path (cons "~/.cabal/bin" load-path))
;;   For formal-ledger-specifications: (setq load-path (cons "~/git/hub/IO/ledger-agda/bin" load-path))
;;   For UALib: (setq load-path (cons "~/git/lab/wjd/UF-Agda_wjd/agda/.cabal-sandbox/bin" load-path))
;; ----------------------------------------
;;
(setenv "LD_LIBRARY_PATH"
  (let ((current (getenv "LD_LIBRARY_PATH"))
;;
;; *** Edit this line ----------
;;   For agda-algebras:
        (new "/usr/local/lib:~/.cabal/lib"))
;;   For formal-ledger-specifications:   (new "/usr/local/lib:~/git/hub/IO/ledger-agda/bin"))
;;   For UALib: (new "/usr/local/lib:~/git/lab/wjd/UF-Agda_wjd/agda/.cabal-sandbox/bin"))
;; -------------------------
;;
(if current (concat new ":" current) new)))
  (load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))
  (set-fontset-font "fontset-default" nil
                  (font-spec :name "DejaVu Sans"))
  (setq auto-mode-alist
    (append
     '(("\\.agda\\'" . agda2-mode)
       ("\\.lagda.md\\'" . agda2-mode))
     auto-mode-alist))
  (add-hook 'agda2-mode-hook (lambda ()
       (setq smartparens-mode nil)
       (setq electric-indent-mode nil) ) )
;;
;; '(agda2-include-dirs (quote ("." "~/git/lab/ualib/ualib.gitlab.io/UALib")))
;; end AGDA-config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'ag)
