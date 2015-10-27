(defvar my-light-theme 'sanityinc-solarized-light)
;(defvar my-dark-theme 'sanityinc-solarized-dark)
(defvar my-dark-theme 'brin)

(defvar current-color-theme my-dark-theme)

(defun set-color-theme (theme)
  (setf current-color-theme theme)
  (load-theme theme t))

;;; Color theme.
(when window-system
  (progn
    (load-theme current-color-theme t)

    (defun light-theme ()
      (interactive)
      (set-color-theme my-light-theme))

    (defun dark-theme ()
      (interactive)
      (set-color-theme my-dark-theme))))

;;; Disable the active color theme for emacsclient frames.
(defun set-emacsclient-colors ()
  (if (display-graphic-p)
      (disable-theme current-color-theme)))

(add-hook 'server-visit-hook 'set-emacsclient-colors)

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
