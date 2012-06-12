;;------------------------------------------------------------------------------
;; General programming settings.
;;------------------------------------------------------------------------------

;; The maximum comment line length.
(setq comment-line-length 80)

;; A map from major modes to comment initializers.
(setq inline-comment-map
      '(("c++-mode"        . "//")
        ("emacs-lisp-mode" . ";;")
        ))

(defun get-inline-comment-init ()
  (let ((value (assoc-string major-mode inline-comment-map)))
    (if (null value) "#" (cdr value))))

(defun insert-comment-line ()
  "Insert a commented line to separate regions of code."
  (interactive)
  (let* ((com (get-inline-comment-init))
         (pos (current-column))
         (chars-to-write (- comment-line-length pos (length com))))
    (beginning-of-line)
    (draw-comment-line com pos chars-to-write "=")))

(defun draw-comment-line (com pos lngth ln)
  (dotimes (i pos) (insert " "))
  (insert com)
  (dotimes (i lngth) (insert ln))
  (insert "\n")
  ;(beginning-of-line)
  ;(previous-line)
  )

(defun auto-close (buf str)
  (if (string-match "exited abnormally" str)
      ;;there were errors
      (message "compilation errors, press C-x ` to visit")
    ;;no errors, make the compilation window go away in 0.5 seconds
    (run-at-time 0.5 nil 'delete-windows-on buf)
    (message "NO COMPILATION ERRORS!")))
