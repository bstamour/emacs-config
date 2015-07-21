;; (setq inferior-lisp-program
;;       (cond (on-windows "sbcl")
;;             ((file-exists-p "/usr/local/bin/sbcl") "sbcl")
;;             ((file-exists-p "/usr/bin/sbcl") "sbcl")
;;             (t "clisp")))


(require 'rainbow-delimiters)
(add-hook 'scheme-mode-hook
	  '(lambda ()
	     (rainbow-delimiters-mode)
	     (paredit-mode)))

(require 'slime)
(setq inferior-lisp-program "sbcl")
(slime-setup '(slime-fancy))

;(load (expand-file-name "~/quicklisp/slime-helper.el"))
(provide 'lang-lisp)
