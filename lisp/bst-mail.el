;;;======================================================================================
;;; GNUS-related settings go here, so that each platform I port my
;;; emacs config to can have a custom .gnus.el file.
;;;
;;; See here
;;; https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/gnus-guide-en.org

;;; MAKE SURE TO CONSTRUCT A .gnus.el FILE ON THIS MACHINE.
;;;
;;; Here is an example config:

;; (setq gnus-select-method
;;       '(nnimap "main"
;; 	       (nnimap-address       "imap.example.com")
;; 	       (nnimap-server-port   993)
;; 	       (nnimap-stream        ssl)
;; 	       ;; see http://www.gnu.org/software/emacs/manual/html_node/gnus/Expiring-Mail.html
;; 	       ;; press 'E' to expire email
;; 	       (nnmail-expiry-target "Trash")
;; 	       (nnmail-expiry-wait   'immediate)))
;;
;; (add-to-list 'gnus-secondary-select-methods
;;              '(nnimap "other"
;;                       (nnimap-address       "imap.example2.com")
;;                       (nnimap-server-port   993)
;;                       (nnimap-stream        ssl)
;; 		      (nnmail-expiry-target "nnimap+other:Trash")
;; 		      (nnmail-expiry-wait   'immediate)))
;;
;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-default-smtp-server "smtp.example.com"
;;       smtpmail-smtp-service 587)
;;;
;;;======================================================================================

(require 'gnus)

;;;---------------------------------------------------------------------------------
;;; Who am I?

(setq user-full-name    "Bryan St. Amour"
      user-mail-address "bryan@bryanstamour.com")

;;;---------------------------------------------------------------------------------
;;; Different posting styles for different groups.
;;;
;;; These are defaults, they can be overridden in the .gnus.el file per-machine.

(setq gnus-posting-styles
      '((".*"
	 (signature "Peace, love, and Unix")
	 (address "bryan@bryanstamour.com")
	 )
	("school"
	 (signature (concat "Bryan St. Amour\n"
			    "BCS, Msc, PhD (student)\n"))
	 (address "stamoub@uwindsor.ca"))
	("work"
	 (signature (concat "Bryan St. Amour\n"
			    "Software Engineer\n"
			    "Tessonics Inc.\n"
			    "519-250-4455 ext 243\n"))
	 (address "bsa@tessonics.com"))))

;;;---------------------------------------------------------------------------------
;;; More settings.

(setq gnus-inhibit-startup-message           t    ; no startup message
      gnus-treat-display-smileys             nil  ; no smileys
      message-kill-buffer-on-exit            t    ; no hanging mail buffers

      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
      gnus-thread-ignore-subject             t
      gnus-thread-hide-subtree               t    ; no threads in summary
      gnus-fetch-old-headers t

      gnus-prompt-before-saving              t    ; better than default
      message-send-mail-partially-limit      nil  ; size of sent messages
      gnus-large-newsgroup                   1000
      gnus-use-full-window                   nil
      )

;;;---------------------------------------------------------------------------------
;;; Special keyboard shortcuts.

(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5))

;; list all the subscribed groups even they contain zero un-read messages
(define-key gnus-group-mode-map   (kbd "o") 'my-gnus-group-list-subscribed-groups)
(define-key gnus-summary-mode-map (kbd "S") 'gnus-summary-move-article)
(define-key gnus-summary-mode-map (kbd "r") 'gnus-summary-wide-reply)
(define-key gnus-summary-mode-map (kbd "R") 'gnus-summary-wide-reply-with-original)

;;;---------------------------------------------------------------------------------
;;; Support for BBDB, and database for storing contact info.
;;;
;;; BBDB stores contact info in the file ~/.emacs.d/bbdb and
;;; allows me to auto-complete contacts by typing in their
;;; email addresses and then just hitting <TAB>. Very handy.

(add-hook 'message-mode-hook
	  '(lambda ()
	     (bbdb-initialize 'message)
	     (bbdb-initialize 'gnus)
	     (local-set-key "<TAB>" 'bbdb-complete-name)))

(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

(provide 'bst-mail)
