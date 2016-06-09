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

(provide 'bst-common-lisp)
