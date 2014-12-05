(setq inferior-lisp-program
      (cond (on-windows "sbcl")
            ((file-exists-p "/usr/local/bin/sbcl") "sbcl")
            ((file-exists-p "/usr/bin/sbcl") "sbcl")
            (t "clisp")))

(require 'slime)
(slime-setup '(slime-fancy))

(provide 'bst-lang-lisp)
