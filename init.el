;;; Main emacs config file.

(defvar on-windows (string= system-type "windows-nt"))
(defvar on-laptop (string= system-name "cantor.thestamours.net"))

;;; Global path variables.
(defvar root-dir (file-name-directory load-file-name))
(defvar third-party-dir (concat root-dir "third-party/"))
(defvar elpa-dir (concat root-dir "elpa/"))
(defvar custom-dir (concat root-dir "custom/"))
(defvar personal-dir (concat root-dir "personal/"))

;;; Add them all to the load path.
;(add-to-list 'load-path root-dir)
(add-to-list 'load-path third-party-dir)
(add-to-list 'load-path elpa-dir)
(add-to-list 'load-path custom-dir)
(add-to-list 'load-path personal-dir)

(let ((default-directory elpa-dir))
  (normal-top-level-add-subdirs-to-load-path))

(let ((default-directory third-party-dir))
  (normal-top-level-add-subdirs-to-load-path))

(setq custom-file (concat personal-dir "custom.el"))
(setq functions-file (concat personal-dir "functions.el"))

(require 'package)
(package-initialize)
(mapc
 (lambda (repo) (add-to-list 'package-archives repo) t)
 '(("marmalade" . "http://marmalade-repo.org/packages/")
   ("melpa" . "http://melpa.milkbox.net/packages/")))

(unless package-archive-contents
  (package-refresh-contents))

(setq package-list
      '(
	;; Color themes.
	autumn-light-theme
	color-theme-sanityinc-solarized
	;; Language support.
	haskell-mode
	csharp-mode
	php-mode
	slime
	tuareg
	web-mode
	;; Utils.
	magit
	geben
	git-commit-mode
	git-rebase-mode
	w3m
	cl-lib
	))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'style)
(require 'keybindings)
(require 'editing)

(require 'lang-cpp)
(require 'lang-haskell)
(require 'lang-web)

(if on-windows
    (require 'windows))

(eshell)
