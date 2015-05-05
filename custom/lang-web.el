(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.php" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ctp" . web-mode))

(autoload 'web-mode "Web editing")

(setq web-mode-code-indent-offset 2)

(add-hook 'web-mode-hook
	  '(lambda ()
	     (setq tab-width 2)
	     (setq indent-tabs-mode t)
	     (define-key web-mode-map (kbd "TAB") 'self-insert-command)))

(provide 'lang-web)
