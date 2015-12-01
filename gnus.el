(setq gnus-select-method
      '(nnimap "personal"
	       (nnimap-address "minerva.bryanstamour.com")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)
	       ;; @see http://www.gnu.org/software/emacs/manual/html_node/gnus/Expiring-Mail.html
	       ;; press 'E' to expire email
	       (nnmail-expiry-target "Trash")
	       (nnmail-expiry-wait 'immediate)))

(add-to-list 'gnus-secondary-select-methods
             '(nnimap "work"
                      (nnimap-address "secure116.inmotionhosting.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
		      (nnmail-expiry-target "nnimap+work:INBOX/Trash")
		      (nnmail-expiry-wait 'immediate)))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("minerva.bryanstamour.com" 587 nil nil))
      smtpmail-auth-credentials     '(("minerva.bryanstamour.com" 587 "bryan" nil))
      smtpmail-default-smtp-server  "minerva.bryanstamour.com"
      smtpmail-smtp-server          "minerva.bryanstamour.com"
      smtpmail-smtp-service         587)

;;; Who am I?
(setq user-full-name "Bryan St. Amour")
(setq user-mail-address "bryan@bryanstamour.com")

(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5))

(define-key gnus-group-mode-map
  ;; list all the subscribed groups even they contain zero un-read messages
  (kbd "o") 'my-gnus-group-list-subscribed-groups)

(add-hook 'message-mode-hook
	  '(lambda ()
	     (bbdb-initialize 'message)
	     (bbdb-initialize 'gnus)
	     (local-set-key "<TAB>" 'bbdb-complete-name)))

(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
