(add-hook 'LaTeX-mode-hook (lambda ()
			     (push
			      '("Latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
				:help "Run Latexmk on file")
			      TeX-command-list)
			     (define-key 'LaTeX-mode-map "\C-cb" 'ebib-insert-bibtex-key)
			     (setq TeX-command-default "Latexmk")))

(provide 'bst-tex)
