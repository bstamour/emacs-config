
;;; Color theme.

(defvar dark-theme  'brin)
(defvar light-theme 'autumn-light)

(load-theme dark-theme t t)
(load-theme light-theme t t)

(defun light-theme ()
  (interactive)
  (enable-theme light-theme))

(defun dark-theme ()
  (interactive)
  (enable-theme dark-theme))

(dark-theme)

(setq inhibit-startup-message t
      initial-scratch-message nil
      visible-bell            t)

(show-paren-mode t)
(column-number-mode t)
(iswitchb-mode 1)

;;; Remove scrollbars and menus.

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode)   (menu-bar-mode -1))

(set-default-font "Monospace-8")

(provide 'bst-style)
