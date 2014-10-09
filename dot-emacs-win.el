;; Sets your shell to use cygwin's bash, if Emacs finds it's running
;; under Windows and c:\cygwin exists. Assumes that C:\cygwin\bin is
;; not already in your Windows Path (it generally should not be).
;;
;; (let* ((cygwin-root "c:/cygwin64")
;;        (cygwin-bin (concat cygwin-root "/bin")))
;;   (when (and (eq 'windows-nt system-type)
;;        (file-readable-p cygwin-root))
;;
;;     (setq exec-path (cons cygwin-bin exec-path))
;;     (setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))
;;
;;     ;; By default use the Windows HOME.
;;     ;; Otherwise, uncomment below to set a HOME
;;     ;;      (setenv "HOME" (concat cygwin-root "/home/eric"))
;;
;;     ;; NT-emacs assumes a Windows shell. Change to bash.
;;     (setq shell-file-name "bash")
;;     (setenv "SHELL" shell-file-name)
;;     (setq explicit-shell-file-name shell-file-name)
;;
;;     ;; This removes unsightly ^M characters that would otherwise
;;     ;; appear in the output of java applications.
;;     (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))
(add-to-list 'load-path (getenv "HOME"))
(require 'cygwin-mount)
(cygwin-mount-activate)
;; exec-path to go first
(add-to-list 'exec-path "C:/cygwin64/bin")
(add-to-list 'exec-path "C:/Program Files/R/R-3.0.2/bin")
(add-to-list 'exec-path "C:/Program Files/R/R-3.0.2/bin/x64")
(add-to-list 'exec-path "C:/apps/Python27")
(add-to-list 'exec-path "C:/PROGRA~2/Git/bin/")
;; entry to the new
(load "C:/dot-emacs/srimacs/init.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-list (quote (("acrobat reader" "evince %o"))))
 '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and start") (output-dvi "Yap") (output-pdf "acrobat reader") (output-html "start"))))
 '(bmkp-last-as-first-bookmark-file "c:/Users/srirampc/AppData/Roaming/.emacs.d/bookmarks")
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(haskell-notify-p t)
 '(haskell-process-type (quote ghci))
 '(haskell-stylish-on-save t)
 '(haskell-tags-on-save t)
 '(sclang-auto-scroll-post-buffer t)
 '(sclang-eval-line-forward nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mumamo-background-chunk-major ((((class color) (background dark)) (:background "black")))))
