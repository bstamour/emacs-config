;; Check the OS and system.
(defvar on-windows (string= system-type "windows-nt"))
(defvar on-laptop (string= system-name "slacktop.slacktown"))

;; Directory structure.
(defvar root-dir (file-name-directory load-file-name))
(defvar modules-dir (concat root-dir "modules/"))
(defvar vendor-dir (concat root-dir "vendor/"))
(defvar personal-dir (concat root-dir "personal/"))
(defvar elpa-dir (concat root-dir "elpa/"))
(defvar theme-dir (concat root-dir "themes/"))

;; Add them all to the load path.
(add-to-list 'load-path modules-dir)
(add-to-list 'load-path vendor-dir)
(add-to-list 'load-path personal-dir)
(add-to-list 'load-path elpa-dir)

(when (>= emacs-major-version 24)
  (add-to-list 'custom-theme-load-path theme-dir))

;; Add all subdirs of elpa to the load path as well.
(let ((default-directory elpa-dir))
  (normal-top-level-add-subdirs-to-load-path))

;; Same with vendor.
(let ((default-directory vendor-dir))
  (normal-top-level-add-subdirs-to-load-path))

;; Config changes made through the customize UI.
(setq custom-file (concat personal-dir "custom.el"))
(setq functions-file (concat personal-dir "functions.el"))

;; Package manager settings.

(defvar my-required-packages
  '(caml
    cl-lib
    color-theme
    color-theme-monokai
    csharp-mode
    git-commit-mode
    git-rebase-mode
    haskell-mode
    magit
    slime
    tuareg
    w3m
    web-mode))

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)

  (mapc
   (lambda (repo) (add-to-list 'package-archives repo) t)
   '(("marmalade" . "http://marmalade-repo.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/")))

  (mapc (lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
        my-required-packages))

;; Load the configs.
(require 'core)
(require 'lang-c)
(require 'lang-lisp)
(require 'lang-web)
(require 'lang-cs)

(when on-windows
  (require 'windows))

;; Finally, a shell.
(eshell)
(gnus)
