;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs config file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Update the load path.
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/color-theme")
(add-to-list 'load-path "~/.emacs.d/slime")
(add-to-list 'load-path "~/.emacs.d/custom-color-themes")
(add-to-list 'load-path "~/.emacs.d/etc")


;; A boolean flag to determine if I am on my Macbook or not.
(setq on-laptop (equal (system-name) "Bryan-St-Amours-MacBook.local"))


;; On the school server?
(setq on-school-server (equal (system-name) "bravo"))


;; Load my custom files.
(load "c-like-languages.el")
(load "web-dev.el")
(load "other-languages.el")
(load "other-customizations.el")

















