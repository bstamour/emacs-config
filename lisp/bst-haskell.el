
(setf haskell-process-path-stack "C:/Users/bryan/AppData/Roaming/local/bin/stack")
(setf haskell-process-type 'stack-ghci)
(setf haskell-process-suggest-remove-import-lines t)
(setf haskell-process-auto-import-loaded-modules t)
(setf haskell-process-log t)

(add-to-list 'auto-mode-alist '("\\.hs" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs" . literate-haskell-mode))

(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-x C-d") nil)
     (define-key haskell-mode-map (kbd "C-c C-z")
       'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-l")
       'haskell-process-load-file)
     (define-key haskell-mode-map (kbd "C-c C-b")
       'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-t")
       'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-i")
       'haskell-process-do-info)
     ))

(provide 'bst-haskell)
