;(load "haskell-mode/haskell-site-file")
;(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)



(mmm-add-classes
 '((literate-haskell-bird
    :submode text-mode
    :front "^[^>]"
    :include-front true
    :back "^>\\|$"
    )
   (literate-haskell-latex
    :submode literate-haskell-mode
    :front "^\\\\begin{code}"
    :front-offset (end-of-line 1)
    :back "^\\\\end{code}"
    :include-back nil
    :back-offset (beginning-of-line -1)
    )))

(defun my-mmm-mode ()
  ;; go into mmm minor mode when class is given
  (make-local-variable 'mmm-global-mode)
  (setq mmm-global-mode 'true))

(setq mmm-submode-decoration-level 0)

(add-hook 'haskell-mode-hook '(lambda ()
                                (turn-on-haskell-indentation)
                                (my-mmm-mode)))
(add-hook 'literate-haskell-mode-hook '(lambda ()
                                         (turn-on-haskell-indentation)
                                         (my-mmm-mode)))




;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(provide 'lang-haskell)
