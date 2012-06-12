;; Haskell editing support.
(load "haskell-site-file")
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

;; Basic java style for when I have to use it...
(c-add-style "my-java-style"
             '("java"
               (c-basic-offset . 2)))
(add-hook 'java-mode (lambda () (c-set-style "my-java-style")))

;; C.
(add-hook 'c-mode (lambda () (c-set-style "bsd")))
