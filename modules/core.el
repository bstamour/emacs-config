(require 'cl)
(require 'color-theme)
(require 'color-theme-monokai)

(if window-system
    (color-theme-monokai))

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

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

(set-frame-font "Monospace-10" t)

(if (not on-windows)
    (progn
      (setenv-from-shell-environment)
      (setq exec-path (split-string (getenv "PATH") path-separator))))

(provide 'core)
