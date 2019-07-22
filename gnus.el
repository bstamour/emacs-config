;;;;============================================================================
;;;;
;;;;                            gnus config.
;;;;
;;;;============================================================================

;;;;============================================================================
;;;; account details

(setq user-mail-address "bryan@stamour.io"
      user-full-name    "Bryan St. Amour")

(setq gnus-select-method
      '(nnimap "personal"
	       (nnimap-address "worker.bryanstamour.com")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)))

;;;;============================================================================
;;;; config

(add-hook 'gnus-summary-exit-hook (lambda () (delete-window)))

;; TODO: Clean this up. Make a list for each address I want to support.
;; Then, loop through it and fill gnus-secondary-select-methods.
(setq default-smtp-server "worker.bryanstamour.com"
      default-stream-type 'starttls
      default-service 587)

(setq message-send-mail-function   'smtpmail-send-it
      smtpmail-default-smtp-server default-smtp-server
      smtpmail-stream-type         default-stream-type
      smtpmail-smtp-service        default-service
      gnus-gcc-mark-as-read        t)

(setq gnus-parameters
      '((".*" (gcc-self . "Sent"))))

(setq gnus-posting-styles
      `(
	(".*"
	 (address ,user-mail-address))
	("^ISO"
	 (address "bryan@bryanstamour.com"))
	("school:"
	 (address "stamoub@uwindsor.ca"))
	))

;; The ISO generates a lot of traffic. Expire these after a while.
(setq gnus-auto-expirable-newsgroups    "ISO\\.*"
      nnmail-expiry-wait                28)  ; expire after a month (give or take)

;;;;============================================================================
;;;; Fancy styles.
(gnus-add-configuration
 '(article
   (horizontal 1.0
	       (vertical 0.25
			 (group 1.0))
               (vertical 1.0
                         (summary 0.25 point)
                         (article 1.0)))))
(gnus-add-configuration
 '(summary
   (horizontal 1.0
               (vertical 0.25
                         (group 1.0))
               (vertical 1.0
                         (summary 1.0 point)))))

;;;;============================================================================
;;;; mail topology

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(eval-after-load 'gnus-topic
  '(progn
     (setq gnus-topic-topology '(("Gnus" visible)
				 (("Email" visible nil nil))
				 (("ISO" visible nil nil))
				 (("Mailings" visible nil nil))))
     (setq gnus-topic-alist '(("Email"
			       "INBOX"
			       "Sent"
			       "Spam")
			      ("ISO"
			       "ISO"
			       "ISO.Lib"
			       "ISO.Core"
			       "ISO.Parallel"
			       "ISO.SCC"
			       "ISO.Sci"
			       "ISO.Lib-Ext"
			       "ISO.Ext"
			       "ISO.Edit"
			       "ISO.Discussion"
			       "ISO.All"
			       "ISO.Direction"
			       "ISO.Filesystem"
			       "ISO.Meeting"
			       "ISO.Modules"
			       "ISO.Networking"
			       "ISO.News"
			       )
			      ("Mailings"
			       "UAI"
			       "arxiv")
			      ("Gnus")))))    ; Anything not listed gets tossed
					; into the gnus top-level

;;;;============================================================================
