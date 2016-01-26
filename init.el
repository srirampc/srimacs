;;; init.el --
;;
;; Bootstrap to load other files
;;
;; Install latest org-mode > 8.2 before we do anything
(package-initialize)

;; load other files from the `after-init-hook' so all packages are loaded
;; load the starter kit from the `after-init-hook' so all packages are loaded
;; (add-hook 'after-init-hook
;;  `(lambda ()
;;     ;; remember this directory
;;     (setq srimacs-root-dir
;;           ,(file-name-directory (or load-file-name (buffer-file-name))))
;;     ;; load up the starter kit
;;     (org-babel-load-file (expand-file-name "srimacs.org" srimacs-root-dir))))

(add-hook 'after-init-hook
 `(lambda ()
    ;; remember this directory
    (setq srimacs-root-dir
          ,(file-name-directory (or load-file-name (buffer-file-name))))
    ;; load up the starter kit
    (load (expand-file-name "srimacs.el" srimacs-root-dir))
    ))
