;(load "haskell-mode/haskell-site-file")
;(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)


(add-hook 'haskell-mode-hook '(lambda ()
                                (turn-on-haskell-indentation)))

(add-hook 'literate-haskell-mode-hook '(lambda ()
                                         (turn-on-haskell-indentation)))


(add-to-list 'auto-mode-alist '("\\.hs" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs" . literate-haskell-mode))

;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(provide 'bst-lang-haskell)
