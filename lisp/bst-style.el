
;;; Color theme.
(defvar light-theme 'solarized-light)
(defvar dark-theme  'solarized-dark)

(defun light-theme ()
  (interactive)
  (load-theme light-theme t))

(defun dark-theme ()
  (interactive)
  (load-theme dark-theme t))

(dark-theme)

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq visible-bell t)
(show-paren-mode t)
(column-number-mode t)
(iswitchb-mode 1)

;;; Remove scrollbars and menus.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(set-default-font "Monospace-8")

(provide 'bst-style)
