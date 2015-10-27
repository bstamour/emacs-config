
(setq backup-directory-alist `(("." . "~/.saves")))

(require 'bookmark+)

(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace)))

(provide 'editing)
