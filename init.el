(add-to-list 'load-path "~/.emacs.d/lisp")

(defvar on-windows (string= system-type "windows-nt"))

(require 'package)

(package-initialize)
(mapc
 (lambda (repo) (add-to-list 'package-archives repo) t)
 '(
   ;("marmalade" . "http://marmalade-repo.org/packages/")
   ("melpa" . "http://melpa.milkbox.net/packages/")
   ))

(require 'bst-style)
(if on-windows
    (require 'bst-windows))

(require 'bst-keybindings)
(require 'bst-editing)
(require 'bst-org)
(require 'bst-haskell)
(require 'bst-common-lisp)
(require 'bst-scheme)
(require 'bst-cpp)
(require 'bst-web)
(require 'bst-git)
(require 'bst-mail)
(require 'bst-erc)
;(require 'bst-mu4e)
;(require 'bst-elfeed)
(require 'bst-tex)
(require 'bst-blogging)

(require 'fireplace)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
