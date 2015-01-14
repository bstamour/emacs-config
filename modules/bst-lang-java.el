(require 'cc-mode)

(c-add-style "my-java-style"
	     '("java"
	       (c-basic-offset . 2)
	       (c-offsets-alist
		(substatement-open . 0))))

(add-hook 'java-mode-hook
	  (lambda ()
	    (c-set-style "my-java-style")))

(provide 'bst-lang-java)
