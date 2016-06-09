
(add-hook 'LaTeX-mode-hook (lambda ()
			     (push
			      '("Latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
				:help "Run Latexmk on file")
			      TeX-command-list)
			     (setq TeX-command-default "Latexmk")))

(provide 'bst-tex)
