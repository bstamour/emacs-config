;;------------------------------------------------------------------------------
;; C-style language settings.
;;------------------------------------------------------------------------------

(require 'cc-mode)

;; Custom C++ style.
(c-add-style "my-c++-style"
             '("stroustrup"
               (c-basic-offset . 2)
               (c-offsets-alist
                (inline-open   . 0)
                (arglist-close . 0)
                (innamespace . 0)
                )))
(add-hook 'c++-mode (lambda () (c-set-style "my-c++-style")))

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
(add-hook 'c++-mode-hook 'set-default-compile-command)

(add-hook 'c++-mode-hook
          (lambda ()
            (global-set-key "\C-c\C-j" 'insert-comment-line)
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
