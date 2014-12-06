(if on-laptop
    (progn
      (setq gnus-select-method
	    '(nnimap "local"
		     (nnimap-address "localhost")
		     (nnimap-server-port 143)
		     (nnimap-stream network)))
      (setq message-send-mail-function 'message-send-mail-with-sendmail)
      (setq sendmail-program "/usr/local/bin/msmtp-enqueue.sh"))
  (progn
    (setq gnus-select-method
          '(nnimap "main"
                   (nnimap-address "mailserver.bryanstamour.com")
                   (nnimap-server-port 993)
                   (nnimap-stream ssl)))
    (setq message-send-mail-function 'smtpmail-send-it
          smtpmail-starttls-credentials '(("mailserver.bryanstamour.com" 465 nil nil))
          smtpmail-auth-credentials '(("mailserver.bryanstamour.com" 465
                                       "bryan" nil))
          smtpmail-default-smtp-server "mailserver.bryanstamour.com"
          smtpmail-smtp-server "mailserver.bryanstamour.com"
          smtpmail-smtp-service 465)))

;;------------------------------------------------------------------------------------

;; Shortcuts to make gnus behave more like pine. Taken from here:
;;
;;    http://www.emacswiki.org/emacs/GnusAndPine
;;

;;     n p    go to next or previous article
;;     d      delete article, move to next
;;     c      compose mail
;;     r      reply
;;     R      reply with citation
;;     s      save article and delete it
;;     x      prompt for immediate deletion of the article
;;     g      goto other group by specifying its short name
;;     u      clear mark, move to next article
;;     ?      view help
;;     !      tick article as important, move to next article
;;     v      view attachment
;;     l      exit summary buffer
;;     TAB    go to next unread article
;;     f      forward mail

(add-hook 'gnus-summary-mode-hook
	  (lambda ()
	    (local-set-key (kbd "<tab>") 'gnus-summary-next-unread-article)
	    (local-set-key "="  'toggle-article-window)
	    (local-set-key "n"  'gnus-summary-next-article)
	    (local-set-key "p"  'gnus-summary-prev-article)
	    (local-set-key "!"  'gnus-summary-put-mark-as-ticked-next)
	    (local-set-key "d"  'gnus-summary-delete-article)
	    (local-set-key "u"  'gnus-summary-clear-mark-forward)
	    (local-set-key "r"  'gnus-summary-dwim-reply)
	    (local-set-key "R"  'gnus-summary-dwim-reply-with-original)
	    (local-set-key "x"  'gnus-summary-delete-article)
	    (local-set-key "g"  'gnus-summary-goto-group)
	    (local-set-key "?"  'gnus-info-find-node)
	    (local-set-key "l"  'gnus-summary-exit)
	    (local-set-key "s"  'gnus-summary-move-article)
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

(add-hook 'gnus-article-mode-hook
	  (lambda ()
	    (local-set-key "r"  'gnus-summary-dwim-reply)
	    (local-set-key "R"  'gnus-summary-dwim-reply-with-original)))

(defun gnus-summary-goto-group (my-group)
  "Prompt for a group short name and open it in summary buffer.
  Default is next group showing in the *Group* buffer with unread articles."
  (interactive
   (list (read-string
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

;;------------------------------------------------------------------------------------

;; Additional settings.

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

;; Delete messages instead of expire them.
(add-hook 'gnus-summary-mode-hook
	  (lambda ()
	    (unless (gnus-news-group-p gnus-newsgroup-name)
	      (set (make-local-variable  'gnus-expirable-mark) ?D)
	      (set (make-local-variable  'gnus-canceled-mark)  ?X)
	      (set (make-local-variable  'gnus-ancient-mark)   ? )
	      (set (make-local-variable  'gnus-read-mark)      ? ))))
(setq nnmail-expiry-wait 0)

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

(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

;; Render html messages using w3m.
(setq mm-text-html-renderer 'w3m)

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-date))
(setq gnus-article-sort-functions
      '(gnus-article-sort-by-date))
