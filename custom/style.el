
;;; Color theme.
(load-theme 'sanityinc-solarized-dark t)

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

(provide 'style)
