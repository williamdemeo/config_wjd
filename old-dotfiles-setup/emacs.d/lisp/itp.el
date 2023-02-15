; FILE: itp.el
; BLAME: <williamdemeo@gmail.com>
; DESC: customizations for itp with emacs
; DATE: 23 Mar 2020
; UPDATES:
;  23 Mar 2020 <williamdemeo@gmail.com>

(message "Loading itp.el")


;-- Proof General ------------------------------------------------------------------
 (load-file "/usr/share/emacs/site-lisp/proofgeneral/generic/proof-site.el")
 ;;(load-file "~/opt/ProofGeneral/generic/proof-site.el")

;-- Agda ----------------------------------------------------------------------------
 (setq load-path (cons "~/git/lab/wjd/UF-Agda_wjd/agda/.cabal-sandbox/bin/" load-path))

 (set-fontset-font "fontset-default" nil (font-spec :name "DejaVu Sans"))

 (setenv "LD_LIBRARY_PATH"
  (let(
       (current (getenv "LD_LIBRARY_PATH"))
       (new "/usr/local/lib:~/git/lab/wjd/UF-Agda_wjd/agda/.cabal-sandbox/bin")
      )
   (if current (concat new ":" current) new)
  ))

  (load-file (let ((coding-system-for-read 'utf-8))
		  (shell-command-to-string "agda-mode locate")))

  (setq auto-mode-alist
     (append
       '(("\\.agda\\'" . agda2-mode)
	 ("\\.lagda.md\\'" . agda2-mode))
       auto-mode-alist))



;-- Lean -------------------------------------------------------------------
 ; If you want to use Lean,
 ;
 ; 1. What are you doing here?
 ; 2. Microsoft VS Code is probably the IDE you want.
 ; 3. Go away!
 ; 4. Are you still here?  Okay, then uncomment the next three lines.
 ;    (setq lean-rootdir "~/git/PROGRAMMING/LEAN/lean")
 ;    (setq load-path (cons "~/git/PROGRAMMING/LEAN/lean/src/emacs" load-path))
 ;    (require 'lean-mode)
