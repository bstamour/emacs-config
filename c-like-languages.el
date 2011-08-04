;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ programming customizations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'cc-mode)


;; Custom C++ style.
(c-add-style "my-style"
             '("bsd"
               (c-basic-offset . 2)
               (c-offsets-alist
                (innamespace . -)
                (inline-open . 0)
                (inher-cont . c-lineup-multi-inher)
                (arglist-cont-nonempty . +)
                (arglist-close . 0)
                (template-args-cont . +))))


;; Set default styles for languages.
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (c-mode . "bsd")
                        (c++-mode . "my-style")))


;; Spaces instead of tabs.
(setq-default indent-tabs-mode nil)
