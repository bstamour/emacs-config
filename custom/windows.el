;;; Windows-specific customizations.

;; default Latin font (e.g. Consolas)
(set-face-attribute 'default nil :family "Consolas")

;; default font size (point * 10)
;;
;; WARNING!  Depending on the default font,
;; if the size is not supported very well, the frame will be clipped
;; so that the beginning of the buffer may not be visible correctly.
(set-face-attribute 'default nil :height 120)

(setq default-directory "~/")

(provide 'windows)
