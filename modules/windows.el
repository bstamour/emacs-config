(setq default-directory "C:/Users/Bryan/")

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(set-face-attribute 'default nil :font "Courier New-10")

(provide 'windows)
