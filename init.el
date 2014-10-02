;;; init.el -- 
;;
;; Bootstrap to load other files  
;;
;; Install org-mode > 8.2 before we do anything
(package-initialize)
(require 'org-install)
(require 'ob-tangle)

;; load other files from the `after-init-hook' so all packages are loaded

;; load the starter kit from the `after-init-hook' so all packages are loaded
(add-hook 'after-init-hook
 `(lambda ()
    ;; remember this directory
    (setq srimacs-root-dir
          ,(file-name-directory (or load-file-name (buffer-file-name))))
    ;; load up the starter kit
    (org-babel-load-file (expand-file-name "srimacs.org" srimacs-root-dir))))

