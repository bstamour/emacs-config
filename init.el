;;;;============================================================================
;;;;
;;;;                   Bryan St. Amour's emacs config
;;;;
;;;;============================================================================

;;;-----------------------------------------------------------------------------
;;; Preamble.
;;;-----------------------------------------------------------------------------

(defvar on-windows (string= system-type "windows-nt"))
(defvar on-laptop  (string= system-name "bryan-laptop"))

(require 'tramp)
(require 'package)

(setq package-enable-at-startup nil)
(package-initialize)
(mapc
 (lambda (repo) (add-to-list 'package-archives repo) t)
 '(
   ("marmalade" . "http://marmalade-repo.org/packages/")
   ("melpa" . "http://melpa.milkbox.net/packages/")
   ))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/cc-mode-snapshot/")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;;;-----------------------------------------------------------------------------
;;; look and feel.
;;;-----------------------------------------------------------------------------

;; Color theme.
(defvar *dark-theme* 'solarized-dark)
(defvar *light-theme* 'solarized-light)
(defvar *current-theme* nil)

(load-theme *dark-theme* t t)
(load-theme *light-theme* t t)

(defun enable-light-theme ()
  (interactive)
  (setf *current-theme* *light-theme*)
  (enable-theme *light-theme*))

(defun enable-dark-theme ()
  (interactive)
  (setf *current-theme* *dark-theme*)
  (enable-theme *dark-theme*))

(defun toggle-themes ()
  (interactive)
  (if (eq *current-theme* *dark-theme*)
      (enable-light-theme)
    (enable-dark-theme)))

(enable-dark-theme)

(setq inhibit-startup-message t
      initial-scratch-message nil
      visible-bell            t)

(show-paren-mode t)
(column-number-mode t)
(iswitchb-mode 1)

;; Remove scrollbars and menus.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode)   (menu-bar-mode -1))

(set-default-font "Monospace-8")

;;;-----------------------------------------------------------------------------
;;; Keybindings
;;;-----------------------------------------------------------------------------

(global-set-key "\C-x\C-b" 'electric-buffer-list)

(global-set-key (kbd "<f5>") 'magit-status)
(global-set-key (kbd "<f6>") 'toggle-themes)
(global-set-key (kbd "<f7>") (lambda ()
			       (interactive)
			       (find-file "~/.emacs.d/init.el")))

(global-set-key (kbd "<f8>") 'bjm/elfeed-load-db-and-open)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(defalias 'qrr 'query-replace-regexp)
(defalias 'yes-or-no-p 'y-or-n-p)

;;;-----------------------------------------------------------------------------
;;; Editing.
;;;-----------------------------------------------------------------------------

(setq backup-directory-alist `(("." . "~/.saves")))

(setq version-control t                    ; Use version numbers for backups
       kept-new-versions 16                ; Number of newest versions to keep
       kept-old-versions 2                 ; Number of oldest versions to keep
       delete-old-versions t               ; Ask to delete excess backup versions?
       backup-by-copying-when-linked t)    ; Copy linked files, don't rename.

 (defun force-backup-of-buffer ()
   (let ((buffer-backed-up nil))
     (backup-buffer)))

;; Make sure dired doesn't get too cluttered up.
(require 'dired-x)
(setq dired-omit-mode t)

(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace)
	    (force-backup-of-buffer)))

;; vlf is for editing really big files. It opens them in chunks.
;; See here: https://github.com/m00natic/vlfi for info.
(require 'vlf-setup)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(define-key key-translation-map (kbd "C-c C-;") (kbd "◊"))

;; For editing text.
(require 'writegood-mode)

;;;-----------------------------------------------------------------------------
;;; org-mode config
;;;-----------------------------------------------------------------------------

(require 'org)

(setq org-agenda-files (list "~/Dropbox/org"))

(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "HIGH-PRIORITY" "ON-HOLD" "|" "DONE" "DELEGATED")))

;;;-----------------------------------------------------------------------------
;;; Haskell config.
;;;-----------------------------------------------------------------------------

(setf haskell-process-path-stack
      (if on-windows
	  "C:/Users/bryan/AppData/Roaming/local/bin/stack"
	"stack"))

(setf haskell-process-type 'stack-ghci)
(setq haskell-process-args-stack-ghci '("--ghci-options=-ferror-spans"))
(setf haskell-process-suggest-remove-import-lines t)
(setf haskell-process-auto-import-loaded-modules t)
(setf haskell-process-log t)

(add-to-list 'auto-mode-alist '("\\.hs" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs" . literate-haskell-mode))

(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-x C-d") nil)
     (define-key haskell-mode-map (kbd "C-c C-z")
       'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-l")
       'haskell-process-load-file)
     (define-key haskell-mode-map (kbd "C-c C-b")
       'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-t")
       'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-i")
       'haskell-process-do-info)
     ))

;;;-----------------------------------------------------------------------------
;;; Common lisp config
;;;-----------------------------------------------------------------------------

(if on-windows
    (add-to-list 'exec-path
		 "C:\\Program Files\\Steel Bank Common Lisp\\1.3.6"))

(setq inferior-lisp-program
      (if (executable-find "sbcl")
	  "sbcl"
	"clisp"))

(require 'slime)
(slime-setup '(slime-fancy))

;;;-----------------------------------------------------------------------------
;;; scheme config
;;;-----------------------------------------------------------------------------

(require 'geiser-repl)

(if on-windows
    (setq geiser-racket-binary "C:/Program Files/Racket/Racket.exe"))

;;;-----------------------------------------------------------------------------
;;; C++ config
;;;-----------------------------------------------------------------------------

(require 'cc-mode)

;; Change the font to "comment" inside an #if 0 #endif block.
(defun my-c-mode-font-lock-if (limit)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) token comment-start comment-start-depth if-stack)
        (flet ((start-comment
                ()
                ;; Check if a comment can be started now and
		;; if so, do it.
                (when (null comment-start)
                  (setq comment-start (match-end 0))
                  (setq comment-start-depth depth)))

               (finish-comment
                ()
                ;; Check if a comment is being closed off now,
		;; if so, close it and do the font trickery.
                (when (and
                       (not (null comment-start))
                       (= depth comment-start-depth))
                  (c-put-font-lock-face
                   comment-start
                   (match-beginning 0)
                   'font-lock-comment-face)
                  (setq comment-start nil))))

          (while (re-search-forward
                  "^\\s-*#\\s-*\\(if\\|else\\|endif\\)"
                  limit
                  'move)
            (setq token (match-string 1))

            (cond ((string= token "if")
                   (setq depth (1+ depth))
                   (cond ((looking-at "\\s-+0") ; Found an #if 0
                          (push 0 if-stack)
                          (start-comment))
                         ((looking-at "\\s-+1") ; Found an #if 1
                          (push 1 if-stack))
                         (t
                          (push 2 if-stack))))  ; Found an #if cond

                  ((string= token "else")
                   (let ((stack-top (pop if-stack)))
                     (cond ((= 0 stack-top) ; Closing an #if 0
                            (finish-comment)
                            (push 1 if-stack))
                           ((= 1 stack-top) ; Closing an #if 1
                            (start-comment)
                            (push 0 if-stack))
                           (t
                            (push stack-top if-stack)))))

                  ((string= token "endif")
                   (finish-comment)
                   (setq depth (1- depth))
                   (pop if-stack))))

        (when (and comment-start (> depth 0))
          (c-put-font-lock-face
           comment-start (point)
           'font-lock-comment-face))))))
  nil)

;; Activate it all.
(add-hook 'c++-mode-hook
	  '(lambda ()
	     ;; Turn off complex indentation and just indent to the previous line.
	     (setq c-basic-offset 2)
	     (setq c-syntactic-indentation nil)
	     (local-set-key (kbd "<enter>") 'electric-newline-and-maybe-indent)
	     (font-lock-add-keywords
	      nil
	      '((my-c-mode-font-lock-if
		 (0
		  font-lock-comment-face prepend)))
	      'add-to-end)))

;;;-----------------------------------------------------------------------------
;;; web editing
;;;-----------------------------------------------------------------------------

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.php" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ctp" . web-mode))

(autoload 'web-mode "Web editing")

(setq web-mode-code-indent-offset 2)

(add-hook 'web-mode-hook
	  '(lambda ()
	     (setq tab-width 2)
	     (setq indent-tabs-mode t)
	     (define-key web-mode-map (kbd "TAB") 'self-insert-command)))

;;;-----------------------------------------------------------------------------
;;; Perl 6
;;;-----------------------------------------------------------------------------

(setq perl6-indent-offset 2)

;;;-----------------------------------------------------------------------------
;;; git
;;;-----------------------------------------------------------------------------

(setq magit-auto-revert-mode nil)

;;;-----------------------------------------------------------------------------
;;; LaTeX
;;;-----------------------------------------------------------------------------

(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (push
	     '("Latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
	       :help "Run Latexmk on file")
	     TeX-command-list)
	    (define-key 'LaTeX-mode-map "\C-cb" 'ebib-insert-bibtex-key)
	    (setq TeX-command-default "Latexmk")))

;;;-----------------------------------------------------------------------------
;;; Big brother database (bbdb) for storing contacts.
;;;-----------------------------------------------------------------------------

(require 'bbdb)

;; initialization
(bbdb-initialize 'gnus 'message)
(bbdb-mua-auto-update-init 'gnus 'message)

;; size of the bbdb popup
(setq bbdb-pop-up-window-size 0.15)
(setq bbdb-mua-pop-up-window-size 0.15)
(setq bbdb-file "~/Dropbox/emacs/bbdb")

;; What do we do when invoking bbdb interactively
(setq bbdb-mua-update-interactive-p '(query . create))

;; Make sure we look at every address in a message and not only the
;; first one
(setq bbdb-message-all-addresses t)

;; use ; on a message to invoke bbdb interactively
(add-hook
 'gnus-summary-mode-hook
 (lambda ()
    (define-key gnus-summary-mode-map (kbd ";") 'bbdb-mua-edit-field)))

(add-hook 'bbdb-create-hook 'bbdb-save)

;;;-----------------------------------------------------------------------------
;;; RSS Settings.
;;;-----------------------------------------------------------------------------

(setf elfeed-db-directory "~/Dropbox/emacs/elfeed")

(setq elfeed-feeds
      '(
	;; Normal news.
	"http://rss.cnn.com/rss/cnn_topstories.rss"
	"http://rss.nytimes.com/services/xml/rss/nyt/US.xml"

	;; Blogs/tech
	"https://planet.haskell.org/rss20.xml"
	"https://isocpp.org/blog/rss"
	"http://alien.slackbook.org/blog/feed/"

	;; Science news
	"https://phys.org/rss-feed/physics-news/"
	"https://phys.org/rss-feed/space-news/"
	"https://phys.org/rss-feed/technology-news/"
	))

(defun elfeed-mark-all-as-read ()
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

;;functions to support syncing .elfeed between machines
;;makes sure elfeed reads index from disk before launching
(defun bjm/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force))

;;write to disk when quiting
(defun bjm/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

;(defalias 'elfeed-toggle-star
;  (elfeed-expose #'elfeed-search-toggle-all 'star))

(use-package elfeed
  :ensure t
  :bind (:map elfeed-search-mode-map
	      ("q" . bjm/elfeed-save-db-and-bury)
	      ("Q" . bjm/elfeed-save-db-and-bury)
	      ("m" . elfeed-toggle-star)
	      ("M" . elfeed-toggle-star)
	      )
  )

(use-package elfeed-goodies
  :ensure t
  :config
  (elfeed-goodies/setup))

;(use-package elfeed-org
;  :ensure t
;  :config
;  (elfeed-org)
;  (setq rmh-elfeed-org-files (list "~/Dropbox/emacs/elfeed.org")))

;;;-----------------------------------------------------------------------------
;;; IRC settings.
;;;-----------------------------------------------------------------------------

(setq erc-nick "bstamour")

;;;-----------------------------------------------------------------------------
;;; web browsing
;;;-----------------------------------------------------------------------------

(defun my-browse-firefox (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (if (not on-windows)
      (browse-url-firefox url new-window)
    (let ((cmd "C:/Program Files (x86)/Mozilla Firefox/firefox"))
      (message "Starting firefox...")
      (start-process (concat cmd " " url) nil cmd url)
      (message "starting firefox... done"))))

(setq browse-url-browser-function 'my-browse-firefox
      browse-url-new-window-flag  t
      browse-url-firefox-new-window-is-tab t)

;;;-----------------------------------------------------------------------------
;;; Windows related stuff.
;;;-----------------------------------------------------------------------------

(when on-windows
  (progn
    ;; default Latin font (e.g. Consolas)
    (set-face-attribute 'default nil :family "Consolas")

    ;; default font size (point * 10)
    ;;
    ;; WARNING!  Depending on the default font,
    ;; if the size is not supported very well, the frame will be clipped
    ;; so that the beginning of the buffer may not be visible correctly.
    (set-face-attribute 'default nil :height 100)

    (setq default-directory "~/")

    ;; Use PuTTY as a backend for tramp.
    (setq tramp-default-method "pscp")

    ;; Help clean up delphi error dumps.
    (defun delphi-cleaner ()
      (interactive)
      (beginning-of-buffer)
      (replace-regexp "  " "\n")
      (beginning-of-buffer))
    ))

;;;-----------------------------------------------------------------------------
;;; This is the end.
;;;-----------------------------------------------------------------------------
