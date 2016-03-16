(require 'geiser-repl)

(defun my-pretty-lambda ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955) ; λ
          )))

(add-hook 'geiser-repl-mode-hook
	  (lambda ()
	    (prettify-symbols-mode)
	    (my-pretty-lambda)))

;(global-prettify-symbols-mode 1)

(provide 'bst-scheme)
