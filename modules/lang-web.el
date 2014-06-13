(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.php" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ctp" . web-mode))

(autoload 'web-mode "Mode for generic web editing.")

(defun my-web-mode-hook ()
  ;; Indentation should not be less than tab-width, so set tab-width to 2.
  (setq tab-width 2)
  (setq indent-tabs-mode t))

(add-hook 'web-mode-hook 'my-web-mode-hook)

(provide 'lang-web)
