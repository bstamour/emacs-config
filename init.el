;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs config file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Update the load path.
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/color-theme")
(add-to-list 'load-path "~/.emacs.d/slime")
(add-to-list 'load-path "~/.emacs.d/custom-color-themes")
(add-to-list 'load-path "~/.emacs.d/etc")


;; A boolean flag to determine if I am on my Macbook or not.
(setq on-laptop (equal (system-name) "Bryan-St-Amours-MacBook.local"))


;; On the school server?
(setq on-school-server (or (equal (system-name) "bravo")
                           (equal (system-name) "alpha")
                           (equal (system-name) "charlie")
                           (equal (system-name) "luna")
                           (equal (system-name) "sol")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ programming customizations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'cc-mode)


;; Custom C++ style.
(c-add-style "my-style"
             '("bsd"
               (c-basic-offset . 2)
               (c-offsets-alist
                (innamespace . -)
                (inline-open . 0)
                (inher-cont . c-lineup-multi-inher)
                (arglist-cont-nonempty . +)
                (arglist-close . 0)
                (template-args-cont . +))))


;; Set default styles for languages.
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (c-mode . "bsd")
                        (c++-mode . "my-style")))


;; Spaces instead of tabs.
(setq-default indent-tabs-mode nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support for PHP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; When on the school server nxhtml has problems...
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
(load "haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)


;; Support for lisp.
(setq path-to-sbcl (if on-laptop "/usr/local/bin/sbcl" "/usr/bin/sbcl"))
(setq inferior-lisp-program path-to-sbcl)
(require 'slime)
(slime-setup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other customizations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Sexual color theme.
(require 'color-theme)
(color-theme-initialize)
(require 'billc)
(color-theme-billc)


;; Move buffers around with ease.
(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)


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














