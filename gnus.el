;;;;============================================================================
;;;;
;;;;                            gnus config.
;;;;
;;;;============================================================================

(setq user-mail-address "bryan@bryanstamour.com"
      user-full-name    "Bryan St. Amour")

(setq gnus-select-method
      '(nnimap "personal"
	       (nnimap-address "worker.bryanstamour.com")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)))

(setq gnus-secondary-select-methods
      '((nnimap "school"
		(nnimap-address "imap.gmail.com")
		(nnimap-server-port 993)
		(nnimap-stream ssl))))

;; TODO: Clean this up. Make a list for each address I want to support.
;; Then, loop through it and fill gnus-secondary-select-methods.
(setq default-smtp-server "worker.bryanstamour.com"
      default-stream-type 'starttls
      default-service 587)

(setq message-send-mail-function   'smtpmail-send-it
      smtpmail-default-smtp-server default-smtp-server
      smtpmail-stream-type         default-stream-type
      smtpmail-smtp-service        default-service)

(setq gnus-use-full-window nil)

(setq gnus-posting-styles
      `((".*"
	 (address ,user-mail-address))
	("school:"
	 (address "stamoub@uwindsor.ca"))))

(defun bst-change-smtp ()
  "Change the SMTP server according to the current from line."
  (save-excursion
    (save-restriction
      (message-narrow-to-headers)
      (let* ((from (message-fetch-field "from"))
	     (from-work (string-match "stamoub@uwindsor.ca" from)))    ; is this a "work" From address?
	(setq smtpmail-smtp-service (if from-work 465 default-service) ; the SMTP port at work is different
	      smtpmail-smtp-server (if from-work
	 			       "smtp.gmail.com"                ; the SMTP server at work
	 			     default-smtp-server)              ; the SMTP server otherwise
	      smtpmail-stream-type (if from-work
				       'ssl
				     default-stream-type)
	      smtpmail-default-smtp-server smtpmail-smtp-server)))))

(add-hook 'message-send-hook 'bst-change-smtp)
