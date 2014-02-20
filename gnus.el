
;; When on my laptop, use the local IMAP folder. Else, use IMAP.

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

    (setq message-send-mail-function 'smtpmail-send-it
          smtpmail-starttls-credentials '(("mailserver.bryanstamour.com" 465 nil nil))
          smtpmail-auth-credentials '(("mailserver.bryanstamour.com" 465
                                       "bryan" nil))
          smtpmail-default-smtp-server "mailserver.bryanstamour.com"
          smtpmail-smtp-server "mailserver.bryanstamour.com"
          smtpmail-smtp-service 465)))

;; Default email address.
(setq user-mail-address "bryan@bryanstamour.com")

;; Set the correct email based on who it was sent to.
(setq gnus-posting-styles
      '(((header "to" "stamoub@uwindsor.ca")
         (address "stamoub@uwindsor.ca"))
	((header "cc" "stamoub@uwindsor.ca")
         (address "stamoub@uwindsor.ca"))

        ((header "to" "bsa@tessonics.com")
         (address "bsa@tessonics.com"))
	((header "cc" "bsa@tessonics.com")
         (address "bsa@tessonics.com"))))

;; Show all mail in the inboxes.
(setq gnus-permanently-visible-groups ".*")

(setq gnus-parameters
      '((".*"
         (gnus-show-threads nil)
         (gnus-use-scoring nil)
         (display . all))))

(gnus-demon-add-handler 'gnus-group-get-new-news 5 nil)

(setq gnus-inhibit-startup-message      t    ;; no startup message
      gnus-treat-display-smileys        nil  ;; no smileys
      message-kill-buffer-on-exit       t    ;; no hanging mail buffers
      gnus-thread-hide-subtree          t    ;; no threads in summary
      gnus-prompt-before-saving         t    ;; better than default
      message-send-mail-partially-limit nil  ;; size of sent messages
      gnus-large-newsgroup              1000)

;; default Pine ordered header list when displaying mail
(setq gnus-sorted-header-list
      '("^Date:" "^From:" "^To:" "^Followup-To:" "^Cc:" "Bcc:" "^Newsgroups:" "Fcc:" "^Subject:"))

;; Automatically delete expired emails when we leave the summary buffer.
;(add-hook 'gnus-summary-mode-hook
;          (lambda ()
;            (unless (gnus-news-group-p gnus-newsgroup-name)
;              (set (make-local-variable  'gnus-expirable-mark) ?D)
;              (set (make-local-variable  'gnus-canceled-mark)  ?X)
;              (set (make-local-variable  'gnus-ancient-mark)   ? )
;              (set (make-local-variable  'gnus-read-mark)      ? ))))

(setq nnmail-expiry-wait 0)

;; Render html messages better.
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

(defun gnus-summary-goto-group (my-group)
  "Prompt for a group short name and open it in summary buffer.
  Default is next group showing in the *Group* buffer with unread articles."
  (interactive (list (read-string
                      (format "Go to group (default %s): "
                              (if (eq gnus-keep-same-level 'best)
                                  (gnus-summary-best-group gnus-newsgroup-name)
                                (gnus-group-short-name
                                 (gnus-summary-search-group nil gnus-keep-same-level)))))))
  (if (string= my-group "")
      (gnus-summary-next-group)
    ;; look for group matching the short name, take first match
    (let* ((liste (mapcar 'car gnus-newsrc-alist)) (name))
      (while (and (setq name (pop liste))
                  (not (string-match (concat ":" my-group) name))))
      (setq my-group name))
    (if my-group
        (gnus-summary-read-group my-group t)
      (message "no such group"))))

(defun gnus-summary-dwim-reply ()
  "reply depending on the CC: header"
   (interactive)
   (gnus-with-article-headers
     (cond
       ((not (re-search-forward "^C[Cc]: .\\|^To:.*," nil t))
        (gnus-summary-reply))
       ((y-or-n-p "Reply to all ? ")
        (gnus-summary-wide-reply)
        (goto-char (point-min))
        (flush-lines "^Cc: $"))
       (t (gnus-summary-reply)))
     (message-goto-body)))

(defun gnus-summary-dwim-reply-with-original ()
  "reply, ask all if there is a CC: header or several recipients"
   (interactive)
   (gnus-with-article-headers
     (cond
       ((not (re-search-forward "C[Cc]: .\\|To:.*," nil t))
        (gnus-summary-reply-with-original nil))
       ((y-or-n-p "Reply to all ? ")
        ;; or gnus-summary-very-wide-reply-with-original ?
        (gnus-summary-wide-reply-with-original nil)
        (goto-char (point-min))
        (flush-lines "^Cc: $"))
       (t (gnus-summary-reply-with-original nil)))
     ;; replace next by message-goto-body to get cursor before citation
     (message-goto-signature)))

(defun gnus-summary-save-and-expire (prefix)
   "save and expire article.
  With a prefix N, save and expire the next N articles"
   (interactive "p")
   (save-excursion (gnus-summary-save-article prefix))
   (gnus-summary-put-mark-as-expirable-next prefix))

(defun toggle-article-window ()
   (interactive)
    (if (get-buffer-window "*Article*" nil)
        (gnus-configure-windows 'summary 'force)
      (gnus-configure-windows 'article 'force)))

;; Keyboard shortcuts to make gnus act more like pine.
(add-hook 'gnus-summary-mode-hook
          (lambda ()
            (local-set-key (kbd "<tab>") 'gnus-summary-next-unread-article)
            (local-set-key "="  'toggle-article-window)
            (local-set-key "n"  'gnus-summary-next-article)
            (local-set-key "p"  'gnus-summary-prev-article)
            (local-set-key "!"  'gnus-summary-put-mark-as-ticked-next)
            (local-set-key "d"  'gnus-summary-put-mark-as-expirable-next)
            (local-set-key "u"  'gnus-summary-clear-mark-forward)
            (local-set-key "r"  'gnus-summary-dwim-reply)
            (local-set-key "R"  'gnus-summary-dwim-reply-with-original)
            (local-set-key "x"  'gnus-summary-delete-article)
            (local-set-key "g"  'gnus-summary-goto-group)
            (local-set-key "?"  'gnus-info-find-node)
            (local-set-key "l"  'gnus-summary-exit)
            (local-set-key "s"  'gnus-summary-move-article)
;            (local-set-key "s"  'gnus-summary-save-and-expire)
            (local-set-key "v"  'gnus-article-view-part)
            (local-set-key "c"  'gnus-summary-mail-other-window)
            (local-set-key "$f" 'gnus-summary-sort-by-author)
            (local-set-key "$a" 'gnus-summary-sort-by-original)
            (local-set-key "$d" 'gnus-summary-sort-by-date)
            (local-set-key "$s" 'gnus-summary-sort-by-subject)
            (local-set-key "$z" 'gnus-summary-sort-by-chars)
            (local-set-key "$e" 'gnus-summary-sort-by-score)
            (if (gnus-news-group-p gnus-newsgroup-name)
                (local-set-key "f"  'gnus-summary-followup)
              (local-set-key "f"  'gnus-summary-mail-forward))))