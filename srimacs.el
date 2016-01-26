;; Emacs Customization

;; +Shamelessy Copied+ Heavily Inspired from
;; [[https://github.com/steckerhalter/][steckerhalter]].
;; Uses code from
;; [[https://github.com/overtone/emacs-live][emacs-live]],
;; [[https://github.com/eschulte/emacs24-starter-kit][eschulte]],
;; [[http://tuhdo.github.io/][tuhdo]] and many others.

;; comes with emacs 24.5
;; (if (not (package-installed-p 'auctex))
;;     (package-install 'auctex))

;; Remove the default GNU archives, because we use steckerhalter's =quelpa=.
(setq package-archives nil)

;; quelpa
;; "Build and install your Emacs Lisp packages on-the-fly directly from source"
(if (require 'quelpa nil t)
  (quelpa-self-upgrade)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

;; use-package with quelpa
;; see https://github.com/jwiegley/use-package
(quelpa '(quelpa-use-package :fetcher github :repo "quelpa/quelpa-use-package"))
(require 'quelpa-use-package)

;; Key bindings
(use-package bind-key
  :init
  (defun eval-and-replace ()
    "Replace the preceding sexp with its value."
    (interactive)
    (backward-kill-sexp)
    (condition-case nil
        (prin1 (eval (read (current-kill 0)))
               (current-buffer))
      (error (message "Invalid expression")
             (insert (current-kill 0)))))

  (defun my-indent-whole-buffer ()
    (interactive)
    (indent-region (point-min) (point-max)))

  (defun my-show-file-name ()
    "Show the full path file name in the minibuffer."
    (interactive)
    (message (buffer-file-name))
    (kill-new (file-truename buffer-file-name)))

  (defun my-kill-buffer () (interactive)
         (kill-buffer (buffer-name)))

  (defun my-switch-to-scratch () (interactive)
         (switch-to-buffer "*scratch*"))

  (defun my-switch-buffer () (interactive)
         (switch-to-buffer nil))

  :bind
  (
   ("C-x C-k" . kill-region)
   ("C-c C-k" . kill-region)
   ("C-x C-g" . goto-line)
   ("C-c C-g" . goto-line)
   ("C-w" . backward-kill-word)
   ("C-c n" . my-show-file-name)
   ("<f6>" . my-kill-buffer)
   ("<f8>" . my-switch-buffer)
   ("C-h C-d" . dired-jump)
   ("C-c s b" . speedbar)
   ("C-h C-d" . dired-jump)
   ("C-S-c C-S-c" . mc/edit-lines)
   ("C-<" . mc/mark-previous-like-this)
   ("C->" . mc/mark-next-like-this)
   ("C-*" . mc/mark-all-like-this)
   ("C-h TAB" . my-indent-whole-buffer)
   ("M-9" . my-switch-to-minibuffer-window)
   ("<M-up>" . buf-move-up)
   ("<M-down>" . buf-move-down)
   ("<M-left>" . buf-move-left)
   ("<M-right>" . buf-move-right)
   ("C-h C-z" . projectile-find-file)
   ("C-h G" . projectile-grep)
   ("C-x SPC" . ace-jump-mode)
   ("C-c l n" . linum-mode)
   ("C-h g" . grep-find)
   ("C-h C-o" . occur)))

(require 'uniquify)

(use-package srimacs-settings
  :init
  (setq
   inhibit-startup-message t
   backup-directory-alist `((".*" . ,temporary-file-directory)) ;don't clutter my fs and put backups into tmp
   column-number-mode t                   ;show the column number
   default-major-mode 'text-mode          ;use text mode per default
   mouse-yank-at-point t                  ;middle click with the mouse yanks at point
   history-length 250                     ;default is 30
   locale-coding-system 'utf-8            ;utf-8 is default
   tab-always-indent 'complete            ;try to complete before identing
   confirm-nonexistent-file-or-buffer nil ;don't ask to create a buffer
   recentf-max-saved-items 5000           ;same up to 5000 recent files
   eval-expression-print-length nil       ;do not truncate printed expressions
   eval-expression-print-level nil        ;print nested expressions
   kill-ring-max 5000                     ;truncate kill ring after 5000 entries
   mark-ring-max 5000                     ;truncate mark ring after 5000 entries
   mouse-autoselect-window -.1            ;window focus follows the mouse pointer
   show-paren-delay 0
   ring-bell-function 'ignore             ;ignore the bell
   x-select-enable-clipboard t
   )

  (setq-default
   tab-width 4
   indent-tabs-mode nil                   ;use spaces instead of tabs
   c-basic-offset 4                       ;"tab" with in c-related modes
   c-hungry-delete-key t                  ;delete more than one space
   fill-column 72
   uniquify-buffer-name-style 'post-forward
   uniquify-separator ":"
   )

  ;; All "yes" or "no" questions are aliased to "y" or "n". We don't
  ;; really want to type a full word to answer a question from Emacs,
  ;; yet Emacs imposes that silly behavior on us. No!

  ;; Also Emacs should be able to kill processes without asking which is
  ;; achieved in the second expression. Got that snippet from:
  ;; http://www.masteringemacs.org/articles/2010/11/14/disabling-prompts-emacs/

  (defalias 'yes-or-no-p 'y-or-n-p)

  (provide 'srimacs-settings)

  :config

  (global-auto-revert-mode 1)  ;auto revert buffers when changed on disk
  (show-paren-mode t)          ;visualize()
  ;; (iswitchb-mode t)            ;use advanced tab switching
  (blink-cursor-mode -1)       ;no cursor blinking
  (tool-bar-mode -1)           ;disable the awful toolbar
  (menu-bar-mode -1)           ;no menu, you can toggle it with C-c m
  (scroll-bar-mode -1)         ;disable the sroll bar

  ;;narrow to region should be enabled by default
  (put 'narrow-to-region 'disabled nil)

  (setq kill-buffer-query-functions
        (remq 'process-kill-buffer-query-function
              kill-buffer-query-functions))

  ;; Inconsolata is my favorite font!
  (if (equal (symbol-name system-type) "gnu/linux")
     (set-default-font "Inconsolata-12")
   (if (equal (symbol-name system-type) "windows-nt")
       (set-default-font "Inconsolata-12")
     (set-default-font "Monaco-12")))

  ;;remove all trailing whitespace and trailing blank lines before
  ;;saving the file
  ;; Taken from emacs-live
  (defvar my-ignore-whitespace-modes '(markdown-mode))
  (defun my-cleanup-whitespace ()
    (if (not (member major-mode my-ignore-whitespace-modes))
        (let ((whitespace-style '(trailing empty)) )
          (whitespace-cleanup))))
  (add-hook 'before-save-hook 'my-cleanup-whitespace))

(when (display-graphic-p)
  ;; (use-package grandshell-theme
  ;;   :quelpa
  ;;   (grandshell-theme :repo "steckerhalter/grandshell-theme" :fetcher github))

  (use-package solarized-theme
    :quelpa solarized-theme
    :config
    (load-theme 'solarized-dark))

  ;; (quelpa 'solarized-theme)
  ;; (load-theme 'grandshell t)
  )

(use-package pos-tip
  :quelpa (pos-tip
           :repo "syohex/pos-tip"
           :fetcher github
           :files ("pos-tip.el"))
  :config
  (defun my-show-help ()
  "Show docs for symbol at point or at beginning of list if not
on a symbol. Pass symbol-name to the function DOC-FUNCTION."
  (interactive)
  (let* ((symbol (save-excursion
                   (or (symbol-at-point)
                       (progn (backward-up-list)
                              (forward-char)
                              (symbol-at-point)))))
         (doc-string (if (fboundp symbol)
                         (documentation symbol t)
                       (documentation-property
                        symbol 'variable-documentation t))))
    (if doc-string
        (pos-tip-show doc-string 'popup-tip-face (point) nil -1 60)
      (message "No documentation for %s" symbol))))
  (define-key lisp-mode-shared-map (kbd "C-c C-d")
    (lambda ()
      (interactive)
      (my-show-help))))
;; (require 'pos-tip)


(use-package ace-jump-mode
  :quelpa (ace-jump-mode
           :repo "winterTTr/ace-jump-mode"
           :fetcher github))

(use-package buffer-move
 :quelpa (buffer-move :fetcher wiki))

(use-package bookmark+
  :quelpa
  (bookmark+ :fetcher wiki :files
             ("bookmark+.el" "bookmark+-mac.el" "bookmark+-bmu.el"
              "bookmark+-1.el" "bookmark+-key.el" "bookmark+-lit.el"
              "bookmark+-doc.el" "bookmark+-chg.el")))

(use-package company
  :quelpa (company :repo "company-mode/company-mode" :fetcher github)
  :config
  ;(require 'company)
  (setq
   company-idle-delay 0.3
   company-tooltip-limit 20
   company-minimum-prefix-length 2
   company-echo-delay 0
   company-auto-complete nil)
  (global-company-mode 1)
  (add-to-list 'company-backends 'company-dabbrev t)
  (add-to-list 'company-backends 'company-ispell t)
  (add-to-list 'company-backends 'company-files t)
  (setq company-backends (remove 'company-ropemacs company-backends))

  (defun my-pcomplete-capf ()
    (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))

  (add-hook 'org-mode-hook #'my-pcomplete-capf)
  (use-package company-c-header
    :quelpa (company-c-headers :repo "randomphrase/company-c-headers" :fetcher github)
    :config
    (setq company-backends (delete 'company-semantic company-backends))
    (add-to-list 'company-backends 'company-c-headers)))
;;(define-key c-mode-map  [(tab)] 'company-complete)
;;(define-key c++-mode-map  [(tab)] 'company-complete)

;; (use-package dedicated
;;   :quelpa (dedicated :fetcher github :repo "emacsmirror/dedicated")
;;   ; :config
;;   ; (require 'dedicated)
;;   )


;; Being in a dired buffer it is possible to make the buffer writable and
;; thus rename files and permissions by editing the buffer. Use =C-x C-q=
;; which runs the command =dired-toggle-read-only= to make that possible.

;; =dired-jump= (mapped to =C-h C-d=) jumps to Dired buffer corresponding to
;; current buffer.

(use-package dired+
  :quelpa (dired+ :fetcher wiki)
  :config
  (setq dired-auto-revert-buffer t)
  (setq dired-no-confirm
        '(byte-compile chgrp chmod chown copy delete load move symlink))
  (setq dired-deletion-confirmer (lambda (x) t))
  (setq wdired-allow-to-change-permissions t) ; allow changing of file permissions
  (toggle-diredp-find-file-reuse-dir 1)
  (setq diredp-hide-details-initially-flag nil)
  (setq diredp-hide-details-propagate-flag nil)
  )

;; Many of the gtags setup is taken from
;; http://tuhdo.github.io/c-ide.html

(use-package helm
  :quelpa
  (helm :repo "emacs-helm/helm"
        :fetcher github :files ("*.el" "emacs-helm.sh"))
  :init
  (setq helm-idle-delay 0.1
      helm-input-idle-delay 0.1
      helm-buffer-max-length 50
      helm-M-x-always-save-history t
      helm-buffer-details-flag nil
      helm-mode-handle-completion-in-region nil)
  :bind
  (
   ("M-x" . helm-M-x)
   ("C-h C-h" . helm-M-x)
   ("C-h h" . helm-projectile)
   ("<C-S-iso-lefttab>" . helm-for-files)
   ("C-h ," . helm-apropos)
   ("C-h ." . helm-info-emacs)
   ("C-h 4" . helm-info-elisp)
   ("C-h 3" . helm-locate-library)
   ("C-h C-SPC" . helm-show-kill-ring)
   ("C-h SPC" . helm-all-mark-rings)
   ("C-h C-l" . helm-locate))

  :config
  (require 'helm-config)
  (helm-mode 1)
  (add-to-list 'helm-completing-read-handlers-alist
               '(org-refile)) ; helm-mode does not do org-refile well
  (add-to-list 'helm-completing-read-handlers-alist
               '(org-agenda-refile)) ; same goes for org-agenda-refile
  (add-to-list 'helm-completing-read-handlers-alist '(dired-create-directory))

  (use-package helm-descbinds
    :quelpa (helm-descbinds :repo "emacs-helm/helm-descbinds" :fetcher github)
    :config (helm-descbinds-mode))
  (use-package helm-gtags
    :quelpa (helm-gtags :repo "syohex/emacs-helm-gtags"
                        :fetcher github :files ("helm-gtags.el"))
    :config
    (helm-gtags-mode 1)
    (add-hook 'dired-mode-hook 'helm-gtags-mode)
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (define-key helm-gtags-mode-map (kbd "C-c g a")
      'helm-gtags-tags-in-this-function)
    (define-key helm-gtags-mode-map (kbd "C-c g r")
      'helm-gtags-find-rtag)
    (define-key helm-gtags-mode-map (kbd "M-s")
      'helm-gtags-select)
    (define-key helm-gtags-mode-map (kbd "M-.")
      'helm-gtags-dwim)
    (define-key helm-gtags-mode-map (kbd "M-,")
      'helm-gtags-pop-stack)
    (define-key helm-gtags-mode-map (kbd "C-c <")
      'helm-gtags-previous-history)
    (define-key helm-gtags-mode-map (kbd "C-c >")
      'helm-gtags-next-history)

    (setq helm-gtags-ignore-case t
      helm-gtags-auto-update t
      helm-gtags-prefix-key "\C-cg"))
  (use-package helm-projectile
    :quelpa (helm-projectile :repo "bbatsov/projectile" :fetcher github :files ("helm-projectile.el"))
    :bind ("C-h h" . helm-projectile)))

;;;; iflipb
;; interactively flip between recently visited buffers
(use-package iflipb
  :quelpa (iflipb :repo "jrosdahl/iflipb" :fetcher github)

  :init
  ;; wrap around list
  (setq iflipb-wrap-around t)
  ;; don't ignore buffers starting with * (like magit etc.)
  (setq iflipb-ignore-buffers '("*helm.**"
                                "*magit-diff: .*"
                                "*Quail Completions*"
                                "*Completions*"
                                "*anaconda-mode*"))
  :bind (("<f8>" . iflipb-next-buffer)
         ("<f9>" . iflipb-previous-buffer)))


;(quelpa '(helm-swoop :repo "ShingoFukuyama/helm-swoop" :fetcher github))
;(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

;(quelpa '(howdoi :repo "atykhonov/emacs-howdoi" :fetcher github))

(use-package ido
  :demand
  :init
  (setq ido-enable-flex-matching t
        ido-auto-merge-work-directories-length -1
        ido-create-new-buffer 'always
        ido-everywhere t
        ido-default-buffer-method 'selected-window
        ido-max-prospects 32
        ido-use-filename-at-point 'guess)
  (setq ido-use-faces nil)
  :bind ("C-x C-b" . ido-switch-buffer)
  :config
  (ido-mode 1)

  (use-package flx-ido
    :quelpa (flx-ido :repo "lewang/flx" :fetcher github :files ("flx-ido.el"))
    :config
    (flx-ido-mode 1)))

(use-package move-text
  :quelpa (move-text :fetcher wiki)
  ; :config
  ; (require 'move-text)
  )

(use-package multiple-cursors
 :quelpa (multiple-cursors :fetcher github :repo "magnars/multiple-cursors.el"))

;;
;; Projectile is written by Bozhidar Batsov.
;; Github: https://github.com/bbatsov/projectile
(use-package projectile
  :quelpa (projectile
           :repo "bbatsov/projectile"
           :fetcher github
           :files ("projectile.el"))
  :bind
  (("C-h C-u" . projectile-find-file)
   ("C-h C-y" . projectile-find-dir)
   ("C-h G" . projectile-grep)
   ("C-h z" . projectile-ack)
   ("C-h C-p" . projectile-switch-project))

  :config
  (projectile-global-mode 1)
  )
;(require 'projectile nil t)

;; Keep track of recently opened files
(use-package recentf
 :config
 (setq recentf-save-file (expand-file-name "~/.recentf"))
 (recentf-mode 1))

;; Save point for files previously opened
(use-package saveplace
  :config
  (setq-default save-place t))
;(require 'saveplace)
;(setq-default save-place t)

(use-package shell-switcher
  :quelpa
  (shell-switcher :fetcher github
          :repo "DamienCassou/shell-switcher"
          :files ("rswitcher.el" "shell-switcher.el"))
  :config
  (setq shell-switcher-new-shell-function 'shell-switcher-make-ansi-term)
  (setq shell-switcher-mode t))

;(require 'shell-switcher)

;; Smart Line Mode is written by Artur Bruce-Connor. The default Emacs
;; mode-line has some shortcomings and =sml= does a good job at
;; improving it.
;; Github:
;; https://github.com/Bruce-Connor/smart-mode-line
(use-package smart-mode-line
  :quelpa
  (smart-mode-line :repo "Bruce-Connor/smart-mode-line" :fetcher github)
  :init
  (setq sml/vc-mode-show-backend t)
  (setq sml/no-confirm-load-theme t)
  :config
  (sml/setup)
  (sml/apply-theme 'respectful)
  (use-package smart-mode-line-powerline-theme
    :quelpa
    (smart-mode-line-powerline-theme
     :repo "Bruce-Connor/smart-mode-line"
     :fetcher github
     :files ("themes/smart-mode-line-powerline-theme.el")))
  (sml/apply-theme 'powerline))

;; Speed bar ?
(use-package sr-speedbar
  :quelpa
  (sr-speedbar :fetcher wiki))

;; Open large files
(use-package vlf
  :quelpa
  (vlf :repo "m00natic/vlfi" :fetcher github :old-names (vlfi))
  :init
  (setq vlf-application 'dont-ask)        ; just do it
  (setq vlf-batch-size 8192)              ; a bit more text per batch please
  ;; :config
  ;;(require 'vlf-integrate)                ; just do it for real
  )

;; yasnippet : snippets
(use-package yasnippet
  :quelpa
  (yasnippet :repo "capitaomorte/yasnippet"
                    :fetcher github
                    :files ("yasnippet.el" "snippets"))
  :config
  (yas-global-mode 1))


(load "auctex")

;;
;; org mode : latex
(use-package org
  :quelpa
  (org :url "git://orgmode.org/org-mode.git" :fetcher git
       :files ("lisp/*.el" "contrib/lisp/*.el" "doc/dir" "doc/*.texi"))
 :config
 (setq org-clock-persist 'history)
 (require 'ox-latex)
 (require 'ox-beamer)

 (defun is-flag-set? (in-str)
   "Check if LEOS flag is set"
   (let ((leos-string
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-min))
              (and (re-search-forward
                    (format "^#\\+%s:[ \t]*\\([-/a-zA-Z]+\\)"
                            in-str) nil t)
                   (match-string 1))))))
     (if (and leos-string
              (string= leos-string "t"))
         t
       nil)))

 (defun is-leos? ()
   "Check if LEOS flag is set"
   (is-flag-set? "LATEX_EXPORT_ON_SAVE"))

 (defun test-leos ()
   (interactive)
   (message (if (is-leos?) "hello" "no!")))

 (defun org-insert-leos-option ()
   "Insert the Latex Export on Save option"
   (interactive)
   (if (not (bolp)) (newline))
   (insert "#+LATEX_EXPORT_ON_SAVE: t"))

 (defun is-beamer-leos? ()
   "Check if LEOS flag is set"
   (is-flag-set? "LATEX_BEAMER_EXPORT_ON_SAVE"))

 (defun org-insert-beamer-leos-option ()
   "Insert the Latex Export on Save option"
   (interactive)
   (if (not (bolp)) (newline))
   (insert "#+LATEX_BEAMER_EXPORT_ON_SAVE: t"))

 (defun org-mode-export-on-save-hook ()
   "Org mode to save as latex hook"
   (if (is-leos?)
       (org-latex-export-to-latex))
   (if (is-beamer-leos?)
       (org-beamer-export-to-latex)))

 (defun org-insert-aps-article-header ()
   "Insert header for APS Report"
   (interactive)
   (if (not (bolp)) (newline))
   (insert "#+TITLE: Annual Progress Seminar Report
#+AUTHOR: Sriram Ponnambalam C (10405602) Guide : Prof. Srinivas Aluru
#+EMAIL:
#+DATE:
#+DESCRIPTION:
#+KEYWORDS:
#+LANGUAGE:  en
#+OPTIONS:   H:3 num:t toc:t \\n:nil @:t ::t |:t ^:t -:t f:t *:t <:t
#+OPTIONS:   TeX:t LaTeX:t skip:nil d:nil todo:t pri:nil tags:not-in-toc
#+INFOJS_OPT: view:nil toc:nil ltoc:t mouse:underline buttons:0 path:http://orgmode.org/org-info.js
#+EXPORT_SELECT_TAGS: export
#+EXPORT_EXCLUDE_TAGS: noexport
#+LINK_UP:
#+LINK_HOME:
#+XSLT:
#+LaTeX_CLASS: aps-article
#+LaTeX_CLASS_OPTIONS: [integrals, nointegrals, article, 11pt, a4paper]
#+LATEX_HEADER: \\usepackage{geometry}
#+LATEX_HEADER: \\usepackage{amsmath}
#+LATEX_HEADER: \\usepackage[MnSymbol]{mathspec}
#+LATEX_HEADER: \\usepackage{fontspec}
#+LATEX_HEADER: \\usepackage{xltxtra}
#+LATEX_HEADER: \\setprimaryfont{Minion Pro}
#+LATEX_HEADER: \\setmainfont[Mapping=tex-text]{Minion Pro}
#+LATEX_HEADER: \\setsansfont[Mapping=tex-text]{Myriad Pro}
#+LATEX_HEADER: \\setmathsfont[Set=Greek,Uppercase=Italic,Lowercase=Italic]{Minion Pro}
#+LATEX_HEADER: \\font\\TitleFont=\"Myriad Pro:letterspace=10,+smcp\" at 24 pt
#+LATEX_HEADER: \\setcounter{secnumdepth}{2}
#+LATEX_HEADER: \\geometry{a4paper, textwidth=6.5in, textheight=10in, marginparsep=7pt, marginparwidth=.6in}
#+LaTeX_HEADER: \\usepackage{amsthm}
#+LaTeX_HEADER: \\newtheorem{theorem}{Theorem}[section]
#+LaTeX_HEADER: \\newtheorem{lemma}[theorem]{Lemma}
#+LATEX_CMD: xelatex
#+LATEX_EXPORT_ON_SAVE: t
"))

 (defun org-insert-memoir-article-header ()
   "Insert header for a Memoir Article"
   (interactive)
   (if (not (bolp)) (newline))
   (insert "#+TITLE: TODO - Insert title
#+AUTHOR: TODO - Insert Author
#+EMAIL:
#+DATE:
#+DESCRIPTION:
#+KEYWORDS:
#+LANGUAGE:  en
#+OPTIONS:   H:3 num:t toc:nil \\n:nil @:t ::t |:t ^:t -:t f:t *:t <:t
#+OPTIONS:   TeX:t LaTeX:t skip:nil d:nil todo:t pri:nil tags:not-in-toc
#+INFOJS_OPT: view:nil toc:nil ltoc:t mouse:underline buttons:0 path:http://orgmode.org/org-info.js
#+EXPORT_SELECT_TAGS: export
#+EXPORT_EXCLUDE_TAGS: noexport
#+LINK_UP:
#+LINK_HOME:
#+XSLT:
#+LaTeX_CLASS: memoir-article
#+LaTeX_CLASS_OPTIONS: [integrals, nointegrals, article, 11pt, a4paper]
#+LATEX_HEADER: \\usepackage{geometry}
#+LATEX_HEADER: \\usepackage{amsmath}
#+LATEX_HEADER: \\usepackage[MnSymbol]{mathspec}
#+LATEX_HEADER: \\usepackage{fontspec}
#+LATEX_HEADER: \\usepackage{xltxtra}
#+LATEX_HEADER: \\setprimaryfont{Minion Pro}
#+LATEX_HEADER: \\setmainfont[Mapping=tex-text]{Minion Pro}
#+LATEX_HEADER: \\setsansfont[Mapping=tex-text]{Myriad Pro}
#+LATEX_HEADER: \\setmathsfont[Set=Greek,Uppercase=Italic,Lowercase=Italic]{Minion Pro}
#+LATEX_HEADER: \\font\\TitleFont=\"Myriad Pro:letterspace=10,+smcp\" at 24 pt
#+LATEX_HEADER: \\setcounter{secnumdepth}{2}
#+LATEX_HEADER: \\geometry{a4paper, textwidth=6.5in, textheight=10in, marginparsep=7pt, marginparwidth=.6in}
#+LaTeX_HEADER: \\usepackage{amsthm}
#+LaTeX_HEADER: \\newtheorem{theorem}{Theorem}[section]
#+LaTeX_HEADER: \\newtheorem{lemma}[theorem]{Lemma}
#+LATEX_CMD: xelatex
#+LATEX_EXPORT_ON_SAVE: t
"))

 (defun org-insert-pdflatex-article-header ()
  "Insert header for Simple pdflatex"
  (interactive)
  (if (not (bolp)) (newline))
  (insert "#+TITLE: TODO - Insert title
#+AUTHOR: TODO - Insert Author
#+EMAIL:
#+DATE:
#+DESCRIPTION:
#+KEYWORDS:
#+LANGUAGE:  en
#+OPTIONS:   H:3 num:t toc:t \\n:nil @:t ::t |:t ^:t -:t f:t *:t <:t
#+OPTIONS:   TeX:t LaTeX:t skip:nil d:nil todo:t pri:nil tags:not-in-toc
#+INFOJS_OPT: view:nil toc:nil ltoc:t mouse:underline buttons:0 path:http://orgmode.org/org-info.js
#+EXPORT_SELECT_TAGS: export
#+EXPORT_EXCLUDE_TAGS: noexport
#+LINK_UP:
#+LINK_HOME:
#+XSLT:
#+LaTeX_CLASS: memoir-pdflatex-article
#+LaTeX_CLASS_OPTIONS: [integrals, nointegrals, article, 11pt, a4paper]
#+LATEX_HEADER: \\usepackage{geometry}
#+LATEX_HEADER: \\usepackage{amsmath}
#+LATEX_HEADER: \\setcounter{secnumdepth}{2}
#+LATEX_HEADER: \\geometry{a4paper, textwidth=6.5in, textheight=10in, marginparsep=7pt, marginparwidth=.6in}
#+LaTeX_HEADER: \\usepackage{amsthm}
#+LaTeX_HEADER: \\newtheorem{theorem}{Theorem}[section]
#+LaTeX_HEADER: \\newtheorem{lemma}[theorem]{Lemma}
#+LATEX_CMD: pdflatex
#+LATEX_EXPORT_ON_SAVE: t
"))

 (defun my-auto-tex-parameters (inarg)
   "Automatically select the tex packages to include."
   ;; default packages for ordinary latex or pdflatex export
   (setq org-latex-default-packages-alist
         '(("AUTO" "inputenc" t)
           ("T1"   "fontenc"   t)
           (""     "fixltx2e"  nil)
           (""     "wrapfig"   nil)
           (""     "soul"      t)
           (""     "textcomp"  t)
           (""     "marvosym"  t)
           (""     "wasysym"   t)
           (""     "latexsym"  t)
           (""     "amssymb"   t)
           (""     "hyperref"  nil)))

   (if (string-match "LATEX_LANG: tamil" (buffer-string))
       (setq org-latex-default-packages-alist
             '(("" "fontspec" t)
               ("" "xunicode" t)
               ("" "xltxtra" t)
               ("xetex" "hyperref" nil)
               )))

   ;; Packages to include when xelatex is used
   (if (string-match "LATEX_CMD: xelatex" (buffer-string))
       (setq org-latex-default-packages-alist
             '(("" "url" t)
               ("" "rotating" t)
               ("american" "babel" t)
               ("babel" "csquotes" t)
               ("" "soul" t)
               ("xetex" "hyperref" nil)
               ))))

 ;; Originally taken from Bruno Tavernier
 ;: http://thread.gmane.org/gmane.emacs.orgmode/31150/focus=31432
 ;; but adapted to use latexmk 4.20 or higher.
 ;; updated to org-mode 8.05
 (defun my-auto-tex-cmd(inarg)
   "When exporting from .org with latex, automatically run latex,
     pdflatex, or xelatex as appropriate, using latexmk."
   (let ((texcmd)))
   ;; default command: oldstyle latex via dvi
   (setq texcmd
         "latexmk -dvi -pdfps -quiet -output-directory=%o %f")
   ;; pdflatex -> .pdf
   (if (string-match "LATEX_CMD: pdflatex" (buffer-string))
       (setq texcmd
             "latexmk -pdf -quiet -output-directory=%o %f"))
   ;; xelatex -> .pdf
   (if (string-match "LATEX_CMD: xelatex" (buffer-string))
       (setq texcmd
             "latexmk -pdflatex=xelatex -pdf -quiet -output-directory=%o %f"))
   ;; LaTeX compilation command
   (setq org-latex-pdf-process (list texcmd)))


 ;; Specify default packages to be included in every tex file, whether
 ;; pdflatex or xelatex
 (setq org-latex-packages-alist
       '(("" "graphicx" t)
         ("" "longtable" nil)
         ("" "float" nil)))

 ;; Languages ??
 (org-babel-do-load-languages 'org-babel-load-languages
                              '((latex . t)))

 ;; Custom article classes
 (add-to-list 'org-latex-classes
             '("xelatex-article"
              "\\documentclass[11pt,article,oneside]{memoir}"
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
              ("\\paragraph{%s}" . "\\paragraph*{%s}")
              ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

 (add-to-list 'org-latex-classes
              '("aps-article"
                "\\documentclass[11pt,article,oneside]{memoir}
              [DEFAULT-PACKAGES]
              [PACKAGES]
              [EXTRA]"
                ("\\chapter{%s}" . "\\chapter*{%s}")
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                ("\\paragraph{%s}" . "\\paragraph*{%s}")
                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

 (add-to-list 'org-latex-classes
              '("kamban-book"
                "\\documentclass[11pt,draft,twoside,a4paper]{kamban}"
                ("\\part{%s}" . "\\part*{%s}")
                ("\\chapter{%s}" . "\\chapter*{%s}")
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                ("\\paragraph{%s}" . "\\paragraph*{%s}")
                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

 (add-to-list 'org-latex-classes
              '("tamil-article"
                "\\documentclass[11pt,article,oneside]{memoir}"
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                ("\\paragraph{%s}" . "\\paragraph*{%s}")
                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

 (add-to-list 'org-latex-classes
              '("memoir-article"
                "\\documentclass[11pt,article,oneside]{memoir}
              [DEFAULT-PACKAGES]
              [PACKAGES]
              [EXTRA]"
                ("\\chapter{%s}" . "\\chapter*{%s}")
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                ("\\paragraph{%s}" . "\\paragraph*{%s}")
                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

 (add-to-list 'org-latex-classes
              '("memoir-pdflatex-article"
                "\\documentclass[11pt,article,oneside]{memoir}
              [DEFAULT-PACKAGES]
              [PACKAGES]
              [EXTRA]"
                ("\\chapter{%s}" . "\\chapter*{%s}")
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                ("\\paragraph{%s}" . "\\paragraph*{%s}")
                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

 (add-hook 'org-mode-hook
          (lambda () (add-hook 'after-save-hook
                               'org-mode-export-on-save-hook
                               nil 'make-it-local)))
 (setq org-export-latex-listings t)
 (add-hook 'org-export-before-processing-hook 'my-auto-tex-cmd)
 ;;(add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-cmd)
 (add-hook 'org-export-before-processing-hook 'my-auto-tex-parameters))


;; mark down mode
(use-package markdown-mode
  :quelpa
  (markdown-mode :url "git://jblevins.org/git/markdown-mode.git" :fetcher git)
  :config
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))

;; CIDER for Clojure
(use-package cider
  :when (and (use-package queue
               :quelpa (queue
                        :url "http://www.dr-qubit.org/predictive/queue.el"
                        :fetcher url
                        :version original)
               :config (featurep 'queue))
             (use-package seq
               :quelpa (seq
                        :repo "NicolasPetton/seq.el"
                        :fetcher github)
               :config (featurep 'seq))
             (use-package spinner
               :quelpa (spinner :repo "Malabarba/spinner.el" :fetcher github)
               :config (featurep 'spinner)))
  :quelpa (cider
           :fetcher github
           :repo "clojure-emacs/cider"
           :files ("*.el" (:exclude ".dir-locals.el"))
           :old-names (nrepl))
  :init
  (setq cider-popup-stacktraces nil)
  (setq cider-repl-popup-stacktraces nil)
  (setq cider-repl-pop-to-buffer-on-connect t)
  (setq cider-repl-use-clojure-font-lock t))

;; ESS - R customization
(use-package ess
  :quelpa
  (ess :repo "emacs-ess/ESS" :fetcher github :files
           ("*.el" ("lisp" "lisp/*.el") ("etc" "etc/*")
            "doc/*.texi" "doc/info/dir"))
  :config
  (require 'ess-site))

(use-package diff-hl
  :quelpa
  (diff-hl :fetcher github :repo "dgutov/diff-hl")
  :config
  (global-diff-hl-mode))

;; Javascript
(use-package js2-mode
  :quelpa
  (js2-mode :repo "mooz/js2-mode" :fetcher github)
  :config
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (add-hook 'js2-mode-hook 'flycheck-mode))

;; Json
(use-package json-mode
  :quelpa
  (json-mode :fetcher github :repo "joshwnj/json-mode")
  :config
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode)))

;; magit
(use-package magit
  :quelpa
  (magit :fetcher github
                :repo "magit/magit"
                :files ("magit.el" "magit-bisect.el" "magit-blame.el"
                        "magit-key-mode.el" "magit-popup.el" "magit-wip.el"
                        "magit.texi" "AUTHORS.md" "README.md"))
  :init
  (setq magit-save-some-buffers nil) ;don't ask to save buffers
  (setq magit-set-upstream-on-push t) ;ask to set upstream
  (setq magit-diff-refine-hunk t) ;show word-based diff for current hunk
  (setq magit-default-tracking-name-function
        'magit-default-tracking-name-branch-only) ;don't track with origin-*
  )

;; (when (fboundp 'file-notify-add-watch)
;;   (quelpa '(magit-filenotify :fetcher github :repo "magit/magit-filenotify"))
;;   (add-hook 'magit-status-mode-hook 'magit-filenotify-mode))

;; magit notify
(use-package magit-filenotify
  ;; Refresh status buffer when git tree changes
  :requires filenotify
  :quelpa (magit-filenotify :fetcher github :repo "magit/magit-filenotify")
  :config (define-key magit-status-mode-map (kbd "`") 'magit-filenotify-mode))

;; General programming to show trailing whitespace
(use-package prog-mode
  :config
  (defun my-prog-mode-hook ()
    (setq show-trailing-whitespace 1))
  (add-hook 'prog-mode-hook 'my-prog-mode-hook))

;; Python
(use-package elpy
  :quelpa (elpy
          :fetcher github
          :repo "jorgenschaefer/elpy"
          :branch "release"
          :files ("elpy.el"
                  "elpy-refactor.el"
                  "elpy-pkg.el.in"
                  "snippets"
                  "elpy"))
  :init
  (elpy-enable))

;; smart parenthesis
(use-package smartparens
  :quelpa (smartparens :fetcher github :repo "Fuco1/smartparens")
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t))


;;;; elec-pair
;; Automatic parenthesis pairing
;; (use-package elec-pair
;;   :config
;;   ;;auto pair brackets, parens etc.
;;   (electric-pair-mode 1))


;;;; Highlight-parentheses
;; (use-package highlight-parentheses
;;   :quelpa (highlight-parentheses :repo "nschum/highlight-parentheses.el"
;;                                  :fetcher github)
;;   :init
;;   (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
;;   (setq hl-paren-delay 0.2)
;;   (setq hl-paren-colors '("Springgreen3"
;;                           "IndianRed1"
;;                           "IndianRed3"
;;                           "IndianRed4"))
;;   :config
;;   (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold))

;; "fix"" highlight issue in scratch buffer
(custom-set-faces '(sp-pair-overlay-face ((t ()))))
