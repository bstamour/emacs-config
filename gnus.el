;;;
;;; My gnus configuration file.
;;;

(if on-laptop
    (progn
      (setq gnus-select-method
            '(nnmaildir "main"
                        (directory "~/Maildir/main/")
                        (directory-files nnheader-directory-files-safe)
                        (get-new-mail nil)))

      (add-to-list 'gnus-secondary-select-methods
                   '(nnmaildir "uwindsor"
                               (directory "~/Maildir/uwindsor/")
                               (directory-files nnheader-directory-files-safe)
                               (get-new-mail nil)))

      ;; Send mail via msmtp-queue.
      (setq message-send-mail-function 'message-send-mail-with-sendmail)
      (setq sendmail-program "/usr/local/bin/msmtp-enqueue.sh"))
  (progn
    ;; Primary email.
    (setq gnus-select-method
          '(nnimap "main"
                   (nnimap-address "mailserver.bryanstamour.com")
                   (nnimap-server-port 993)
                   (nnimap-stream ssl)))

    ;; School email.
    (add-to-list 'gnus-secondary-select-methods
                 '(nnimap "uwindsor"
                          (nnimap-address "imap.gmail.com")
                          (nnimap-server-port 993)
                          (nnimap-stream ssl)))

    ;; Work email.
    (add-to-list 'gnus-secondary-select-methods
                 '(nnimap "tessonics"
                          (nnimap-address "secure116.inmotionhosting.com")
                          (nnimap-server-port 993)
                          (nnimap-stream ssl)))

    (setq message-send-mail-function 'smtpmail-send-it
          smtpmail-starttls-credentials '(("mailserver.bryanstamour.com" 465 nil nil))
          smtpmail-auth-credentials '(("mailserver.bryanstamour.com" 465
                                       "bryan" nil))
          smtpmail-default-smtp-server "mailserver.bryanstamour.com"
          smtpmail-smtp-server "mailserver.bryanstamour.com"
          smtpmail-smtp-service 465)))

(setq user-full-name "Bryan St. Amour")
(setq user-mail-address "bryan@bryanstamour.com")

;; Different posting styles for different groups.
(setq gnus-posting-styles
      '((".*"
	 (signature "Peace, love, and Unix"))
	("uwindsor"
	 (signature "Bryan St. Amour\nBCS\nM.Sc")
	 (address "stamoub@uwindsor.ca"))
	("tessonics"
	 (signature "Bryan St. Amour\nSoftware Engineer")
	 (address "bsa@tessonics.com"))))

;; Don't use the full screen.
(setq gnus-use-full-window nil)

(setq sent-mail-folder (if on-laptop "Sent" "INBOX.Sent"))

(setq gnus-parameters
      `((".*"
         (gnus-show-threads nil)
         (gnus-use-scoring nil)
         (display . all)
         (gcc-self . ,sent-mail-folder)
         )))

(setq gnus-inhibit-startup-message      t    ;; no startup message
      gnus-treat-display-smileys        nil  ;; no smileys
      message-kill-buffer-on-exit       t    ;; no hanging mail buffers
      gnus-thread-hide-subtree          t    ;; no threads in summary
      gnus-prompt-before-saving         t    ;; better than default
      message-send-mail-partially-limit nil  ;; size of sent messages
      gnus-large-newsgroup              1000)

;(setq nnmail-expiry-wait 0)

;; Render html messages using w3m.
(setq mm-text-html-renderer 'w3m)
