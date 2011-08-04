;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support for PHP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; When on the school server nxhtml has problems...
(when (not on-school-server)
  (load "nxhtml/autostart.el")
  ;; For some reason nxhtml likes to shit kittens when it loads,
  ;; this snippet prevents it from doing that...
  (when (and (equal emacs-major-version 23)
             (equal emacs-minor-version 3))
    (eval-after-load "bytecomp"
      '(add-to-list 'byte-compile-not-obsolete-vars
                    'font-lock-beginning-of-syntax-function))
    ;; tramp-compat.el clobbers this variable!
    (eval-after-load "tramp-compat"
      '(add-to-list 'byte-compile-not-obsolete-vars
                    'font-lock-beginning-of-syntax-function)))
  (setq mumamo-background-colors nil))
