(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.php" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ctp" . web-mode))

(autoload 'web-mode "Mode for generic web editing.")

(provide 'lang-web)
