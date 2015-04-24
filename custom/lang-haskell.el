(add-to-list 'auto-mode-alist '("\\.hs" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs" . literate-haskell-mode))

(add-hook 'haskell-mode-hook
	  '(lambda ()
	     (progn
	       (turn-on-haskell-indentation)
	       (turn-on-haskell-doc-mode))))

(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-x C-d") nil)
     (define-key haskell-mode-map (kbd "C-c C-z")
       'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-l")
       'haskell-process-load-or-reload)
     (define-key haskell-mode-map (kbd "C-c C-b")
       'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-t")
       'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-i")
       'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c M-.") nil)
     (define-key haskell-mode-map (kbd "C-c C-d") nil)))

(provide 'lang-haskell)
