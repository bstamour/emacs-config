;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs config file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Update the load path.
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/color-theme")
(add-to-list 'load-path "~/.emacs.d/slime")
(add-to-list 'load-path "~/.emacs.d/darkroom-mode")

;; A boolean flag to determine if I am on my Macbook or not.
(setq on-laptop (equal (system-name) "Bryan-St-Amours-MacBook.local"))

;; On the school server?
(setq on-school-server (equal (system-name) "bravo"))




;; Git support.
(require 'git)
(require 'git-blame)

;; Move buffers around with ease.
(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; ORG Mode.
;(require 'org-install)
;(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;(define-key global-map "\C-cl" 'org-store-link)
;(define-key global-map "\C-ca" 'org-agenda)
;(setq org-log-done t)

;; Sexual color theme.
(require 'color-theme)
(color-theme-initialize)
;(require 'zenburn)
;(color-theme-zenburn)
(require 'billc)
(color-theme-billc)

;(set-background-color "cornsilk")
;(set-foreground-color "black")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ programming customizations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turn off auto-indentation modes for C-like languages.
(require 'cc-mode)
(setq c-basic-offset 2)
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "user")))

(setq basic-offsets-alist '((template-args-cont . +)
                            (arglist-close . 0)))






;; Make the compilation window vanish after 0.5 seconds,
;; unless there is an error.
(setq compilation-window-height 8)
(setq compilation-finish-function
      (lambda (buf str)
        (if (string-match "exited abnormally" str)
            ;;there were errors
            (message "compilation errors, press C-x ` to visit")
          ;;no errors, make the compilation window go away in 0.5 seconds
          (run-at-time 0.5 nil 'delete-windows-on buf)
          (message "NO COMPILATION ERRORS!"))))

;; Spaces instead of tabs.
(setq-default indent-tabs-mode nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support for PHP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (not on-school-server)
  (load "nxhtml/autostart.el")
  ;; For some reason nxhtml likes to shit kittens when it loads,
  ;; this snippet prevents it from doing that...
  (when (and (equal emacs-major-version 23)
             (equal emacs-minor-version 3))
    (eval-after-load "bytecomp"
      '(add-to-list 'byte-compile-not-obsolete-vars
                    'font-lock-beginning-of-syntax-function))
    ;; tramp-compat.el clobbers this variable!
    (eval-after-load "tramp-compat"
      '(add-to-list 'byte-compile-not-obsolete-vars
                    'font-lock-beginning-of-syntax-function)))
  (setq mumamo-background-colors nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other languages.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Haskell editing support.
(load "~/.emacs.d/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Support for lisp.
(setq path-to-sbcl (if on-laptop "/usr/local/bin/sbcl" "/usr/bin/sbcl"))
(setq inferior-lisp-program path-to-sbcl)
(require 'slime)
(slime-setup)

;(require 'php-mode)
;(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
;(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
;(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other customizations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Easier use of M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)

;; Use electric buffer instead of the old shitty buffer list.
(global-set-key "\C-x\C-b" 'electric-buffer-list)
(iswitchb-mode 1)

;; Faster regex replacing.
(defalias 'qrr 'query-replace-regexp)

;; Fix foolish calendar-mode scrolling.
(add-hook 'calendar-load-hook
          '(lambda ()
             (setq mark-holidays-in-calendar t)
             (define-key calendar-mode-map ">" 'scroll-calendar-left)
             (define-key calendar-mode-map "<" 'scroll-calendar-right)
             (define-key calendar-mode-map "\C-x>" 'scroll-calendar-left)
             (define-key calendar-mode-map "\C-x<" 'scroll-calendar-right)))

;; Some laptop-specific configurations.
(if on-laptop
    (progn
      (setq browse-url-browser-function
            'browse-url-default-macosx-browser)
      (setq delete-by-moving-to-trash t)))

;; Turn off the damn bell.
(setq visible-bell t)

;; Highlight matching parenthesis.
(show-paren-mode t)

;; Before saving a file, delete all the trailing whitespace.
(add-hook 'before-save-hook
          (lambda () (delete-trailing-whitespace)))

;; Change the yes/no prompts to y/n instead.
(defalias 'yes-or-no-p 'y-or-n-p)

;; For distraction-free writing.
(require 'darkroom-mode)















