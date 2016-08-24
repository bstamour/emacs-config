;;;-----------------------------------------------------------------------------
;;; Preamble.

(defvar on-windows (string= system-type "windows-nt"))

(require 'package)

(package-initialize)
(mapc
 (lambda (repo) (add-to-list 'package-archives repo) t)
 '(
   ;("marmalade" . "http://marmalade-repo.org/packages/")
   ("melpa" . "http://melpa.milkbox.net/packages/")
   ))

(require 'bst-style)
(if on-windows
    (require 'bst-windows))

(require 'fireplace)

;;;-----------------------------------------------------------------------------
;;; Keybindings

;(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-x\C-b" 'electric-buffer-list)

(defalias 'qrr 'query-replace-regexp)
(defalias 'yes-or-no-p 'y-or-n-p)

;(require 'cl)
;(require 'framemove)
;(windmove-default-keybindings)
;(setq framemove-hook-into-windmove t)


;;;-----------------------------------------------------------------------------
;;; Editing.

(setq backup-directory-alist `(("." . "~/.saves")))

(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace)))

;;; See https://www.emacswiki.org/emacs/InteractivelyDoThings#toc1
(require 'ido)
(ido-mode t)

;;; See https://www.emacswiki.org/emacs/Smex
(global-set-key [(meta x)] (lambda ()
			     (interactive)
			     (or (boundp 'smex-cache)
				 (smex-initialize))
			     (global-set-key [(meta x)] 'smex)
			     (smex)))

(global-set-key (kbd "<f5>") 'magit-status)


(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(define-key key-translation-map (kbd "C-c C-;") (kbd "◊"))






;;;-----------------------------------------------------------------------------
;;; org-mode config

(require 'org)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-agenda-files (list "~/Dropbox/org"))




;;;-----------------------------------------------------------------------------
;;; Haskell config.

(setf haskell-process-path-stack
      (if on-windows
	  "C:/Users/bryan/AppData/Roaming/local/bin/stack"
	"/home/bryan/bin/stack"))

(setf haskell-process-type 'stack-ghci)
(setf haskell-process-suggest-remove-import-lines t)
(setf haskell-process-auto-import-loaded-modules t)
(setf haskell-process-log t)

(add-to-list 'auto-mode-alist '("\\.hs" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs" . literate-haskell-mode))

(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-x C-d") nil)
     (define-key haskell-mode-map (kbd "C-c C-z")
       'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-l")
       'haskell-process-load-file)
     (define-key haskell-mode-map (kbd "C-c C-b")
       'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-t")
       'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-i")
       'haskell-process-do-info)
     ))

;;;-----------------------------------------------------------------------------
;;; Common lisp config

;; (setq inferior-lisp-program
;;       (cond (on-windows "sbcl")
;;             ((file-exists-p "/usr/local/bin/sbcl") "sbcl")
;;             ((file-exists-p "/usr/bin/sbcl") "sbcl")
;;             (t "clisp")))

(add-to-list 'exec-path "C:\\Program Files\\Steel Bank Common Lisp\\1.3.6")
(setq inferior-lisp-program "sbcl")

(require 'slime)
(slime-setup '(slime-fancy))
;(load (expand-file-name "~/quicklisp/slime-helper.el"))


;;;-----------------------------------------------------------------------------
;;; scheme config

(require 'geiser-repl)

(if on-windows
    (setq geiser-racket-binary "C:/Program Files/Racket/Racket.exe"))

;(defun my-pretty-lambda ()
;  "make some word or string show as pretty Unicode symbols"
;  (setq prettify-symbols-alist
;        '(
;          ("lambda" . 955) ; λ
;          )))

;(add-hook 'geiser-repl-mode-hook
;	  (lambda ()
;	    (prettify-symbols-mode)
;	    (my-pretty-lambda)))

;(global-prettify-symbols-mode 1)


;;;-----------------------------------------------------------------------------
;;; C++ config

;; C++ configuration.

(require 'cc-mode)

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
  (add-to-list 'c-offsets-alist
	       '(topmost-intro-cont . align-enum-class))
  (add-to-list 'c-offsets-alist
               '(statement-cont . align-enum-class-closing-brace)))

;; Change the font to "comment" inside an #if 0 #endif block.

(defun my-c-mode-font-lock-if (limit)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) token comment-start comment-start-depth if-stack)
        (flet ((start-comment
                ()
                ;; Check if a comment can be started now and
		;; if so, do it.
                (when (null comment-start)
                  (setq comment-start (match-end 0))
                  (setq comment-start-depth depth)))

               (finish-comment
                ()
                ;; Check if a comment is being closed off now,
		;; if so, close it and do the font trickery.
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
                   (cond ((looking-at "\\s-+0") ; Found an #if 0
                          (push 0 if-stack)
                          (start-comment))
                         ((looking-at "\\s-+1") ; Found an #if 1
                          (push 1 if-stack))
                         (t
                          (push 2 if-stack))))  ; Found an #if cond

                  ((string= token "else")
                   (let ((stack-top (pop if-stack)))
                     (cond ((= 0 stack-top) ; Closing an #if 0
                            (finish-comment)
                            (push 1 if-stack))
                           ((= 1 stack-top) ; Closing an #if 1
                            (start-comment)
                            (push 0 if-stack))
                           (t
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

;; Activate it all.

(add-hook 'c++-mode-hook
	  '(lambda ()
	     (c-set-style "stroustrup")
	     (fix-enum-class)
	     (font-lock-add-keywords
	      nil
	      '((my-c-mode-font-lock-if
		 (0
		  font-lock-comment-face prepend)))
	      'add-to-end)))


;;;-----------------------------------------------------------------------------
;;; web editing

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.php" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ctp" . web-mode))

(autoload 'web-mode "Web editing")

(setq web-mode-code-indent-offset 2)

(add-hook 'web-mode-hook
	  '(lambda ()
	     (setq tab-width 2)
	     (setq indent-tabs-mode t)
	     (define-key web-mode-map (kbd "TAB") 'self-insert-command)))

;;;-----------------------------------------------------------------------------
;;; git

(setq magit-auto-revert-mode nil)

;;;-----------------------------------------------------------------------------
;;; LaTeX

(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (push
	     '("Latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
	       :help "Run Latexmk on file")
	     TeX-command-list)
	    (define-key 'LaTeX-mode-map "\C-cb" 'ebib-insert-bibtex-key)
	    (setq TeX-command-default "Latexmk")))

;;;-----------------------------------------------------------------------------
;;; Functions for creating blog posts with Jekyll.

(setq blog-base-dir "/home/bryan/Projects/personal-site/")
(setq blog-posts-dir (concat blog-base-dir "_posts/"))

(defun new-blog-post (title)
  "Prompt for a blog post title, then create the file with the
proper pre-amble."
  (interactive "sTitle: ")
  (let ((file-name (title-to-file-name title))
	(todays-date (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))
    (find-file (concat blog-posts-dir todays-date "-" file-name))

    (insert "---")
    (newline)
    (insert "layout: post")
    (newline)
    (insert (concat "title: " title))
    (newline)
    (insert "---")

    (newline)
    (newline)))

(defun title-to-file-name (title)
  (concat
   (downcase
    (replace-regexp-in-string
     "[[:space:]]" "-"
     (replace-regexp-in-string "[^[:alnum:][:space:]]" "" title)))
   ".md"))
