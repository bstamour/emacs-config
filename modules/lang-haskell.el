;(load "haskell-mode/haskell-site-file")
;(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)


(add-hook 'haskell-mode-hook '(lambda ()
                                (turn-on-haskell-indentation)))
(add-hook 'literate-haskell-mode-hook '(lambda ()
                                         (turn-on-haskell-indentation)))





;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(provide 'lang-haskell)
