;;------------------------------------------------------------------------------
;; Emacs config file.
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Front matter.
;;------------------------------------------------------------------------------

;; Update the load path.
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/color-theme")
(add-to-list 'load-path "~/.emacs.d/slime")
(add-to-list 'load-path "~/.emacs.d/custom-color-themes")
(add-to-list 'load-path "~/.emacs.d/etc")
(add-to-list 'load-path "~/.emacs.d/tuareg-mode")
(add-to-list 'load-path "~/.emacs.d/theme-changer")

;;------------------------------------------------------------------------------
;; Load the environment variables of the system (assumes I use bash, which
;; I do, so it's all good.)
;;------------------------------------------------------------------------------

;; Apply shell environment to emacs
;; http://paste.lisp.org/display/111574
(require 'cl)

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

(setenv-from-shell-environment)
(setq exec-path (split-string (getenv "PATH") path-separator))

;;------------------------------------------------------------------------------
;; General programming settings.
;;------------------------------------------------------------------------------

;; The maximum comment line length.
(setq comment-line-length 80)

;; A map from major modes to comment initializers.
(setq inline-comment-map
      '(("c++-mode"        . "//")
        ("emacs-lisp-mode" . ";;")
        ))

(defun get-inline-comment-init ()
  (let ((value (assoc-string major-mode inline-comment-map)))
    (if (null value) "#" (cdr value))))

(defun insert-block-comment ()
  "Insert a block comment flower box at the current position."
  (interactive)
  (let* ((com (get-inline-comment-init))
         (pos (current-column))
         (chars-to-write (- comment-line-length pos (length com))))
    (beginning-of-line)
    (draw-comment-line com pos chars-to-write "-")
    (draw-comment-blank-line com pos)
    (draw-comment-line com pos chars-to-write "-")
    (previous-line)             ; Move up two lines and over to the end,
    (previous-line)             ; so the user can start typing the
    (end-of-line)))             ; comment.

(defun draw-comment-line (com pos lngth ln)
  (dotimes (i pos) (insert " "))
  (insert com)
  (dotimes (i lngth) (insert ln))
  (insert "\n"))

(defun draw-comment-blank-line (com pos)
  (dotimes (i pos) (insert " "))
  (insert com)
  (insert " \n"))

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

;;------------------------------------------------------------------------------
;; C-style language settings.
;;------------------------------------------------------------------------------

(require 'cc-mode)

;; Custom C++ style.
(c-add-style "my-c++-style"
             '("stroustrup"
               (c-basic-offset . 2)
               (c-offsets-alist
                (inline-open           . 0)
                (arglist-close         . 0)
                )))

;; Custom Java style.
(c-add-style "my-java-style"
             '("java"
               (c-basic-offset . 2)))

;; Set default styles for languages.
(setq c-default-style '((java-mode . "my-java-style")
                        (awk-mode  . "awk")
                        (c-mode    . "bsd")
                        (c++-mode  . "my-c++-style")))

;; Spaces instead of tabs.
(setq-default indent-tabs-mode nil)

;; Easy way to insert block comments.
(add-hook 'c++-mode-hook
          (lambda ()
            (global-set-key "\C-c\C-j" 'insert-block-comment)
            (global-set-key "\C-c\C-v" 'uncomment-region)
            (global-set-key "\C-c\C-k" 'compile)
            ))

;;------------------------------------------------------------------------------
;; Teach emacs about the new enum class in C++11
;;------------------------------------------------------------------------------

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

(add-hook 'c++-mode-hook 'fix-enum-class)

;;------------------------------------------------------------------------------
;; Highlight regions of code blocked off by #if 0 as if it were a comment.
;;------------------------------------------------------------------------------
(defun my-c-mode-font-lock-if (limit)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) token comment-start comment-start-depth if-stack)
        (flet ((start-comment ()
                              ;; Check if a comment can be started now and if so, do it.
                              (when (null comment-start)
                                (setq comment-start (match-end 0))
                                (setq comment-start-depth depth)))
               (finish-comment ()
                               ;; Check if a comment is being closed off now, if so, close it
                               ;; and do the font trickery.
                               (when (and (not (null comment-start)) (= depth comment-start-depth))
                                 (c-put-font-lock-face
                                  comment-start
                                  (match-beginning 0)
                                  'font-lock-comment-face)
                                 (setq comment-start nil))))

          (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
            (setq token (match-string 1))

            (cond ((string= token "if")
                   (setq depth (1+ depth))
                   (cond ((looking-at "\\s-+0")        ;; Found an #if 0
                          (push 0 if-stack)
                          (start-comment))
                         ((looking-at "\\s-+1")        ;; Found an #if 1
                          (push 1 if-stack))
                         (t
                          (push 2 if-stack))))         ;; Found an #if cond

                  ((string= token "else")
                   (let ((stack-top (pop if-stack)))
                     (cond ((= 0 stack-top)            ;; Closing an #if 0
                            (finish-comment)
                            (push 1 if-stack))
                           ((= 1 stack-top)            ;; Closing an #if 1
                            (start-comment)
                            (push 0 if-stack))
                           (t                          ;; Closing any other cond
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

;;------------------------------------------------------------------------------
;; Other languages.
;;------------------------------------------------------------------------------

;; Haskell editing support.
(load "haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Support for lisp.
(setq inferior-lisp-program "sbcl")
(require 'slime)
(require 'slime-autoloads)
(slime-setup)

;; Support for OCaml editing.
(add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . tuareg-mode))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

;;------------------------------------------------------------------------------
;; Other customizations.
;;------------------------------------------------------------------------------

;; Change the default frame size.
(add-to-list 'default-frame-alist '(width . 100))

;; Color theme.
(require 'color-theme)
(color-theme-initialize)
(color-theme-scintilla)

;; Prevent startup message and switch to empty *scratch*
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Move buffers around with ease.
(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; When on laptop, C-S gets handled by the OS, causing funky things
;; to happen. Use these as a failback.
(global-set-key (kbd "<M-S-up>")     'buf-move-up)
(global-set-key (kbd "<M-S-down>")   'buf-move-down)
(global-set-key (kbd "<M-S-left>")   'buf-move-left)
(global-set-key (kbd "<M-S-right>")  'buf-move-right)

;; Easier use of M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm" 'execute-extended-command)

;; Use electric buffer instead of the old shitty buffer list.
(global-set-key "\C-x\C-b" 'electric-buffer-list)
(iswitchb-mode 1)

;; Easy aliases.
(defalias 'qrr         'query-replace-regexp)
(defalias 'lf          'load-file)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Fix foolish calendar-mode scrolling.
(add-hook 'calendar-load-hook
          '(lambda ()
             (setq mark-holidays-in-calendar t)
             (define-key calendar-mode-map ">"     'scroll-calendar-left)
             (define-key calendar-mode-map "<"     'scroll-calendar-right)
             (define-key calendar-mode-map "\C-x>" 'scroll-calendar-left)
             (define-key calendar-mode-map "\C-x<" 'scroll-calendar-right)))

;; Some laptop-specific configurations.
(if (string-equal system-type "darwin")
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
          (lambda ()
            (delete-trailing-whitespace)
            (delete-trailing-blank-lines)
            ))

 (defun delete-trailing-blank-lines ()
   "Deletes all blank lines at the end of the file, even the last one"
   (interactive)
   (save-excursion
     (save-restriction
       (widen)
       (goto-char (point-max))
       (delete-blank-lines)
       (let ((trailnewlines (abs (skip-chars-backward "\n\t"))))
         (if (> trailnewlines 0)
             (progn
               (delete-char trailnewlines)))))))

;; Set up org-mode for note taking.
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)