(require 'cl)

(defvar emacs-root (if (eq system-type 'darwin)
                       "/Users/bryan/"
                     "/home/bryan/"))

;(add-hook 'kill-emacs-hook
;          (lambda ()
;            (byte-recompile-directory (concat emacs-root ".emacs.d"))))

(labels ((add-to-path (p)
                      (add-to-list
                       'load-path
                       (concat emacs-root p))))
  (add-to-path ".emacs.d")

  (add-to-path ".emacs.d/lisp")
  (add-to-path ".emacs.d/lisp/programming")
  (add-to-path ".emacs.d/lisp/color-themes")

  (add-to-path ".emacs.d/site-lisp")
  (add-to-path ".emacs.d/site-lisp/color-theme")
  (add-to-path ".emacs.d/site-lisp/cc-mode")
  (add-to-path ".emacs.d/site-lisp/slime")
  (add-to-path ".emacs.d/site-lisp/haskell-mode")
  (add-to-path ".emacs.d/site-lisp/nxhtml")
  (add-to-path ".emacs.d/site-lisp/tuareg-mode"))


(add-to-list 'custom-theme-load-path 
	     (concat emacs-root ".emacs.d/lisp/color-themes"))

(load-library "load-shell-env")   ; Copy the shell environment from bash.
(load-library "custom")           ; Generic customizations.

(load-library "prog-general")     ; General programming stuff.
(load-library "prog-cpp")         ; C++ programming.
(load-library "prog-other")       ; Other languages.

(load-library "buffer-move")      ; Swap buffers with easy keystrokes.

(eshell)
