(require 'cl)

(defvar emacs-root (if (eq system-type 'darwin)
                       "/Users/bryan/"
                     "/home/bryan/"))

(labels ((add-to-path (p)
                      (add-to-list
                       'load-path
                       (concat emacs-root p))))
  (add-to-path ".emacs.d")

  (add-to-path ".emacs.d/lisp")
  (add-to-path ".emacs.d/lisp/programming")
  (add-to-path ".emacs.d/lisp/color-themes")
  ;(add-to-path ".emacs.d/lisp/color-themes/emacs-color-theme-solarized")

  (add-to-path ".emacs.d/site-lisp")

  (add-to-path ".emacs.d/site-lisp/games")
  (add-to-path ".emacs.d/site-lisp/games/nethack_0_9_5")

  (add-to-path ".emacs.d/site-lisp/color-theme")
  ;(add-to-path ".emacs.d/site-lisp/cc-mode")
  (add-to-path ".emacs.d/site-lisp/slime")
  (add-to-path ".emacs.d/site-lisp/ess/lisp")
  (add-to-path ".emacs.d/site-lisp/haskell-mode")
  (add-to-path ".emacs.d/site-lisp/nxhtml")
  (add-to-path ".emacs.d/site-lisp/tuareg-mode"))

(add-to-list 'custom-theme-load-path
	     (concat emacs-root
		     ".emacs.d/lisp/color-themes/emacs-color-theme-solarized"))

(load-library "load-shell-env")   ; Copy the shell environment from bash.
(load-library "custom")           ; Generic customizations.

(load-library "prog-general")     ; General programming stuff.
(load-library "prog-cpp")         ; C++ programming.
(load-library "prog-other")       ; Other languages.

(load-library "buffer-move")      ; Swap buffers with easy keystrokes.



(load "~/.emacs.d/site-lisp/nxhtml/autostart")

(server-start)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
