;(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-x\C-b" 'electric-buffer-list)

(defalias 'qrr 'query-replace-regexp)
(defalias 'yes-or-no-p 'y-or-n-p)

;(require 'cl)
;(require 'framemove)
;(windmove-default-keybindings)
;(setq framemove-hook-into-windmove t)

(provide 'bst-keybindings)
