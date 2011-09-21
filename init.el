;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs config file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Front matter.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Update the load path.
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/color-theme")
(add-to-list 'load-path "~/.emacs.d/slime")
(add-to-list 'load-path "~/.emacs.d/ocaml-mode")
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

;; If I kill the buffer containing my init.el file, ask me
;; if I would like to recompile it.
(defun compile-init-file ()
  (interactive)
  (when (and (string-equal
              buffer-file-name
              (expand-file-name "~/.emacs.d/init.el"))
             (y-or-n-p "byte compile init.el? "))
    (byte-compile-file "~/.emacs.d/init.el")))

(add-hook 'kill-buffer-hook 'compile-init-file)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C-style language settings.
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
                        (awk-mode  . "awk")
                        (c-mode    . "bsd")
                        (c++-mode  . "my-style")))

;; Spaces instead of tabs.
(setq-default indent-tabs-mode nil)

;; Change the color of text inside #if 0 /* ... */ #endif
;; to match comments.
;;
;; From
;; http://stackoverflow.com/questions/4549015/in-c-c-mode-in-emacs-change-face-of-code-in-if-0-endif-block-to-comment-fa
(defun my-c-mode-font-lock-if0 (limit)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) str start start-depth)
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
          (setq str (match-string 1))
          (if (string= str "if")
              (progn
                (setq depth (1+ depth))
                (when (and (null start) (looking-at "\\s-+0"))
                  (setq start (match-end 0)
                        start-depth depth)))
            (when (and start (= depth start-depth))
              (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
              (setq start nil))
            (when (string= str "endif")
              (setq depth (1- depth)))))
        (when (and start (> depth 0))
          (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
  nil)
(add-hook 'c-mode-common-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend)))
             'add-to-end)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web development stuff.
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

;; Ocaml syntax highlighting.
(setq auto-mode-alist
          (cons '("\\.ml[iyl]?$" .  caml-mode) auto-mode-alist))
(autoload 'caml-mode "ocaml" (interactive)
  "Major mode for editing Caml code." t)
(autoload 'camldebug "camldebug" (interactive) "Debug caml mode")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General programming settings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Make the compilation window vanish after 0.5 seconds,
;; unless there is an error.
;(setq compilation-window-height 8)
;(setq compilation-finish-functions 'auto-close)

(defun auto-close (buf str)
  (if (string-match "exited abnormally" str)
      ;;there were errors
      (message "compilation errors, press C-x ` to visit")
    ;;no errors, make the compilation window go away in 0.5 seconds
    (run-at-time 0.5 nil 'delete-windows-on buf)
    (message "NO COMPILATION ERRORS!")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other customizations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Sexual color theme.
;(require 'color-theme)
;(load "deeper-blue.el")
;(color-theme-initialize)
;(color-theme-deeper-blue)

;; Move buffers around with ease.
(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; Easier use of M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm" 'execute-extended-command)

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














