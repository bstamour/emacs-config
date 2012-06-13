;; Change the default frame size.
(add-to-list 'default-frame-alist '(width . 80))

;; Prevent startup message and switch to empty *scratch*
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Color theme.
(require 'color-theme)
(color-theme-initialize)

;; If on laptop, use the darker theme. Also should check emacs' version...
;(if (string-equal system-type "darwin")
;    (progn
;      (require 'color-theme-tomorrow)
;      (color-theme-tomorrow-night))
;  (color-theme-wheat))

(require 'color-theme-tomorrow)
(color-theme-tomorrow-night)

;; Turn off the damn bell.
(setq visible-bell t)

;; Highlight matching parenthesis.
(show-paren-mode t)

;; Turn on column numbers.
(column-number-mode t)
;; Move buffers around with ease.
(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; When on laptop, C-S gets handled by the OS, causing funky things
;; to happen. Use these as a failback.
(global-set-key (kbd "<M-S-up>")     'buf-move-up)
(global-set-key (kbd "<M-S-down>")   'buf-move-down)
(global-set-key (kbd "<M-S-left>")   'buf-move-left)
(global-set-key (kbd "<M-S-right>")  'buf-move-right)

;; Easier use of M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm" 'execute-extended-command)

;; Use electric buffer instead of the old shitty buffer list.
(global-set-key "\C-x\C-b" 'electric-buffer-list)
(iswitchb-mode 1)

;; Easy aliases.
(defalias 'qrr         'query-replace-regexp)
(defalias 'lf          'load-file)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Before saving a file, delete all the trailing whitespace.
(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace)
            ))

;; Set up org-mode for note taking.
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; Remove scroll bars and menu bars.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Fix foolish calendar-mode scrolling.
(add-hook 'calendar-load-hook
          '(lambda ()
             (setq mark-holidays-in-calendar t)
             (define-key calendar-mode-map ">"     'scroll-calendar-left)
             (define-key calendar-mode-map "<"     'scroll-calendar-right)
             (define-key calendar-mode-map "\C-x>" 'scroll-calendar-left)
             (define-key calendar-mode-map "\C-x<" 'scroll-calendar-right)))
