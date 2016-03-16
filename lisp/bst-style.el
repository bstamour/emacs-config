
;;; Color theme.

(defvar light-theme 'white-sand)
(defvar dark-theme  'tango-dark)

(defun light-theme ()
  (interactive)
  (load-theme light-theme t))

(defun dark-theme ()
  (interactive)
  (load-theme dark-theme t))

;(if on-windows
;    (light-theme)
;  (dark-theme))

(light-theme)

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
