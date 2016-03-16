
(setq backup-directory-alist `(("." . "~/.saves")))

(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace)))

;;; See https://www.emacswiki.org/emacs/InteractivelyDoThings#toc1
(require 'ido)
(ido-mode t)

;;; See https://www.emacswiki.org/emacs/Smex
(global-set-key [(meta x)] (lambda ()
			     (interactive)
			     (or (boundp 'smex-cache)
				 (smex-initialize))
			     (global-set-key [(meta x)] 'smex)
			     (smex)))

(provide 'bst-editing)
