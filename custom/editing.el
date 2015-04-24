
(setq backup-directory-alist `(("." . "~/.saves")))

(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace)))

(provide 'editing)
