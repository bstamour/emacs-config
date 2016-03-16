(add-to-list 'load-path "~/.emacs.d/lisp")

(defvar on-windows (string= system-type "windows-nt"))

(require 'package)

(package-initialize)
(mapc
 (lambda (repo) (add-to-list 'package-archives repo) t)
 '(("marmalade" . "http://marmalade-repo.org/packages/")
   ("melpa" . "http://melpa.milkbox.net/packages/")))

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

(require 'fireplace)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "01ce486c3a7c8b37cf13f8c95ca4bb3c11413228b35676025fdf239e77019ea1" "df87edcf41dbdb2c5d49d53acdfc9d5c2087ef7259679ac50923f97e0b24fdfe" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
