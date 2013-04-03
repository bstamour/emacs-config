(require 'cl)

(defvar on-windows (if (string= system-type "windows-nt")
		       t
		     nil))

(defvar emacs-root (if on-windows
                       "C:/Users/bryan/AppData/Roaming/"
                     "/home/bryan/"))

(defun add-to-path (p)
  (add-to-list 'load-path (concat emacs-root p)))
(add-to-path ".emacs.d")
(add-to-path ".emacs.d/site-lisp")
(add-to-path ".emacs.d/site-lisp/color-theme")
(add-to-path ".emacs.d/site-lisp/tuareg-mode")
(add-to-path ".emacs.d/site-lisp/haskell-mode")
(add-to-path ".emacs.d/site-lisp/php-mode")
(add-to-path ".emacs.d/site-lisp/slime")

(if on-windows
    (setq default-directory "C:/Users/Bryan/"))

;(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/color-themes/")

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(load-theme 'tango t)

(setq visible-bell t)
(show-paren-mode t)
(column-number-mode t)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-x\C-b" 'electric-buffer-list)

(iswitchb-mode 1)

(defalias 'qrr 'query-replace-regexp)
(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace)))

;; Remove scroll bars and menu bars.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Apply shell environment to emacs
;; http://paste.lisp.org/display/111574
(defun env-line-to-cons (env-line)
  "Convert a string of the form \"VAR=VAL\" to a
cons cell containing (\"VAR\" . \"VAL\")."
  (if (string-match "\\([^=]+\\)=\\(.*\\)" env-line)
    (cons (match-string 1 env-line) (match-string 2 env-line))))

(defun interactive-env-alist (&optional shell-cmd env-cmd)
  "Launch /usr/bin/env or the equivalent from a login
shell, parsing and returning the environment as an alist."
  (let ((cmd (concat (or shell-cmd "$SHELL -lc")
                     " "
                     (or env-cmd "/usr/bin/env"))))
    (mapcar 'env-line-to-cons
            (remove-if
             (lambda (str)
               (string-equal str ""))
             (split-string (shell-command-to-string cmd) "[\r\n]")))))

(defun setenv-from-cons (var-val)
  "Set an environment variable from a cons cell containing
two strings, where the car is the variable name and cdr is
the value, e.g. (\"VAR\" . \"VAL\")"
  (setenv (car var-val) (cdr var-val)))

(defun setenv-from-shell-environment (&optional shell-cmd env-cmd)
  "Apply the environment reported by `/usr/bin/env' (or env-cmd)
as launched by `$SHELL -lc' (or shell-cmd) to the current
environment."
  (mapc 'setenv-from-cons (interactive-env-alist shell-cmd env-cmd)))

(if (not on-windows)
    (progn
      (setenv-from-shell-environment)
      (setq exec-path (split-string (getenv "PATH") path-separator))))

;;---------------------------------------------------------------------------

;; Haskell editing support.
(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Support for lisp.
;(setq inferior-lisp-program "sbcl")
;(require 'slime)
;(require 'slime-autoloads)
;(slime-setup)

;; Support for OCaml editing.
(add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . tuareg-mode))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

(require 'php-mode)

;;---------------------------------------------------------------------------

(require 'cc-mode)

;; Custom C++ style.
;(c-add-style "my-c++-style"
;             '("stroustrup"
;               (c-basic-offset . 2)
;               ))

;; Spaces instead of tabs.
(setq-default indent-tabs-mode nil)

;; Change the default command for M-x compile when in c++-mode.
;; When a makefile exists in the directory just keep things the same.
;; If a makefile doesn't exist, default to running g++-4.7 on the
;; file in the current buffer (with the right flags of course).
;;
;; From http://emacswiki.org/emacs/CompileCommand
(require 'compile)
(defun set-default-compile-command ()
  (interactive)
  (let* ((compiler "g++-4.7")
         (flags    "-std=c++11 -Wall -Werror -Wextra")
         (format-string
          (concat compiler " " flags " "
                  (if (eq system-type 'darwin)
                      "-framework Cocoa"
                    "")
                  " %s -o %s")))
    (unless (file-exists-p "Makefile")
      (set (make-local-variable 'compile-command)
           (let ((file (file-name-nondirectory buffer-file-name)))
             (format format-string
                     file
                     (file-name-sans-extension file)))))))

;; Add support for the 'enum class' keyword in C++11.
(defun inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum[ \t]+class[ \t]+"))))

(defun align-enum-class (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun align-enum-class-closing-brace (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      '-
    '+))

(defun fix-enum-class ()
  "Setup `c++-mode' to better handle \"class enum\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
  (add-to-list 'c-offsets-alist
               '(statement-cont . align-enum-class-closing-brace)))

;; Turn on the fancy C++ settings.
(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "ellemtel")
            (set-default-compile-command)
            (fix-enum-class)
            (global-set-key "\C-c\C-j" 'insert-comment-line)
            (global-set-key "\C-c\C-v" 'uncomment-region)
            (global-set-key "\C-c\C-k" 'compile)
            ))

;;------------------------------------------------------------------------------
;; Highlight regions of code blocked off by #if 0 as if it were a comment.
;;------------------------------------------------------------------------------
(defun my-c-mode-font-lock-if (limit)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) token comment-start comment-start-depth if-stack)
        (flet ((start-comment
                ()
                ;; Check if a comment can be started now and if so, do it.
                (when (null comment-start)
                  (setq comment-start (match-end 0))
                  (setq comment-start-depth depth)))

               (finish-comment
                ()
                ;; Check if a comment is being closed off now, if so, close it
                ;; and do the font trickery.
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
                   (cond ((looking-at "\\s-+0")     ;; Found an #if 0
                          (push 0 if-stack)
                          (start-comment))
                         ((looking-at "\\s-+1")     ;; Found an #if 1
                          (push 1 if-stack))
                         (t
                          (push 2 if-stack))))      ;; Found an #if cond

                  ((string= token "else")
                   (let ((stack-top (pop if-stack)))
                     (cond ((= 0 stack-top)         ;; Closing an #if 0
                            (finish-comment)
                            (push 1 if-stack))
                           ((= 1 stack-top)         ;; Closing an #if 1
                            (start-comment)
                            (push 0 if-stack))
                           (t                       ;; Closing any other cond
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

(add-hook 'c-mode-common-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '((my-c-mode-font-lock-if (0 font-lock-comment-face prepend)))
             'add-to-end)))

;;---------------------------------------------------------------------------

(eshell)
