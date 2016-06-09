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
;(require 'bst-scheme)
(require 'bst-cpp)
(require 'bst-web)
(require 'bst-git)
(require 'bst-mail)
(require 'bst-erc)
;(require 'bst-mu4e)
;(require 'bst-elfeed)
(require 'bst-tex)

(require 'fireplace)
