;;;;============================================================================
;;;;
;;;; Bryan St. Amour's emacs config file.
;;;;
;;;;============================================================================


;;;-----------------------------------------------------------------------------
;;;
;;; shortcuts
;;; ---------
;;;
;;;   F6         magit (git)
;;;   F10        switch color themes


;;;-----------------------------------------------------------------------------
(defvar on-windows (string= system-type "windows-nt"))
(defvar on-laptop  (string= system-name "bryan-laptop"))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)


;;;-----------------------------------------------------------------------------
(require 'package)
(setq package-enable-at-startup nil)
(when (version< emacs-version "27.0") (package-initialize))
(mapc
 (lambda (repo) (add-to-list 'package-archives repo) t)
 '(("gnu"   . "http://elpa.gnu.org/packages/")
   ("melpa" . "http://melpa.milkbox.net/packages/")))


(defun download-use-package ()
  "Call this function to grab use-package from the repos.
Do this once when installing emacs for the first time."
  (interactive)
  (package-initialize)
  (package-refresh-contents)
  (package-install 'use-package))


;;;-----------------------------------------------------------------------------

(use-package solarized-theme
  :ensure t)

(defconst themes '(solarized-dark solarized-light leuven))
(defvar which-theme 0)

(mapc (lambda (theme-name) (load-theme theme-name t t)) themes)

(defun toggle-themes ()
  "Switch beteen a list of themes with a single keystroke."
  (interactive)
  (disable-theme (nth which-theme themes))
  (setq which-theme
	(if (eq which-theme (- (length themes) 1))
	    0
	  (+ which-theme 1)))
  (enable-theme (nth which-theme themes)))
(define-key global-map (kbd "<f10>") 'toggle-themes)

(if (display-graphic-p)
    (enable-theme (nth which-theme themes))
  (load-theme 'wheatgrass))


;;;-----------------------------------------------------------------------------
(defalias 'qrr          'query-replace-regexp)
(defalias 'yes-or-no-p  'y-or-n-p)

(defun gn ()
  "Start gnus in a new fullscreened frame"
  (interactive)
  (gnus-other-frame)
  (toggle-frame-fullscreen))

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode)   (menu-bar-mode -1))

(setq inhibit-startup-message t
      initial-scratch-message nil
      visible-bell            t)

(show-paren-mode t)
(column-number-mode t)
(iswitchb-mode 1)

(add-hook 'before-save-hook
          (lambda () (delete-trailing-whitespace)))

(display-time-mode 1)


;;;-----------------------------------------------------------------------------
(use-package ido
  :config (progn (setq ido-enable-flex-matching t
		       ido-everywhere t)
		 (ido-mode 1)))


;;;-----------------------------------------------------------------------------
(use-package org
  :ensure t
  :bind (("C-c l"   . org-store-link)
	 ("C-c C-l" . org-insert-link)
	 ("C-c a"   . org-agenda)
	 ("C-c c"   . org-capture)
	 ("C-c b"   . org-iswitch))
  :config (setq org-agenda-files      (list "~/Dropbox/wiki")
		org-todo-keywords     '((sequence "TODO" "DELEGATED" "ON-HOLD" "|" "DONE"))
		org-capture-templates '(("t" "Todo [inbox]" entry
					 (file "~/Dropbox/wiki/actions.org")
					 "* TODO %i%?"))))


;;;-----------------------------------------------------------------------------
(use-package magit
  :ensure t
  :bind (("<f6>" . magit-status))
  :config (setq magit-auto-revert-mode nil))


;;;-----------------------------------------------------------------------------
(use-package cc-mode
  :ensure t
  :bind (("<f5>" . compile))
  :hook (c++-mode . (lambda ()
		      ;; Turn off complex indentation and just indent to the previous line.
		      (setq c-basic-offset          2
			    c-syntactic-indentation nil)
		      (local-set-key (kbd "<enter>") 'electric-newline-and-maybe-indent)
		      (local-set-key (kbd "C-M-\\")  'clang-format-region)

		      ;; Special compile command. Unless there's a makefile present,
		      ;; just invoke the compiler on the current file.
		      (unless (or (file-exists-p "makefile")
				  (file-exists-p "Makefile"))
			(set (make-local-variable 'compile-command)
			     (let ((flags "-std=c++17 -O3"))
			       (if on-windows
				   (concat
				    "\"C:/program files/LLVM/bin/clang++\" "
				    "-std=c++17 -O3 -Xclang -flto-visibility-public-std -o "
				    (file-name-base buffer-file-name)
				    ".exe "
				    buffer-file-name)
				 (concat
				  "clang++ -std=c++17 -O3 -o "
				  (file-name-base buffer-file-name)
				  " "
				  buffer-file-name)))))

		      ;; Load up clang-format so we can format the buffer nicely.
		      (load (if on-windows
				"C:/program files/LLVM/share/clang/clang-format.el"
			      "/usr/share/clang/clang-format.el")))))


;;;-----------------------------------------------------------------------------
(use-package slime
  :ensure t
  :defer
  :config (progn
	    (setq inferior-lisp-program
		  (if (executable-find "sbcl")
		      "sbcl"
		    "clisp"))
	    (slime-setup '(slime-fancy))))


;;;-----------------------------------------------------------------------------
(use-package dired
  :config (setq dired-dwim-target t))


;;;-----------------------------------------------------------------------------
(use-package tex
  :ensure auctex
  :defer t
  :config (progn
	    (setq TeX-auto-save  t
		  TeX-parse-self t)
	    (setq-default TeX-engine   'xetex
			  TeX-PDF-mode t)
	    (setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) "Okular")))


;;;-----------------------------------------------------------------------------
(use-package erc
  :config (setq erc-nick "bstamour"))


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
    ;(set-face-attribute 'default nil :height 100)

    (setq default-directory         "~/"
	  ange-ftp-ftp-program-name "C:/Users/Bryan.TESSONICS/ftp-for-win32-1/Release/ftp.exe"
	  tramp-default-method      "pscp")

    (defun delphi-cleaner ()
      "Help clean up delphi error dumps."
      (interactive)
      (beginning-of-buffer)
      (replace-regexp "  " "\n")
      (beginning-of-buffer))))


;;;;============================================================================
