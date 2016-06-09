;;; ebib-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "ebib" "../../../../../.emacs.d/elpa/ebib-20160604.53/ebib.el"
;;;;;;  "fa160a82c63e9635146df480e7d7ca5b")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/ebib-20160604.53/ebib.el

(autoload 'ebib "ebib" "\
Ebib, a BibTeX database manager.
Optional argument FILE is a file to load.  If FILE is already
loaded, switch to it.  If KEY is given, jump to it.

\(fn &optional FILE KEY)" t nil)

(autoload 'ebib-show-entry "ebib" "\
Open Ebib and jump to KEY.

\(fn KEY)" nil nil)

(defalias 'ebib-open-org-link 'ebib-show-entry "\
Open Ebib and jump to KEY.
This is for use in Org-mode links.")

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/elpa/ebib-20160604.53/ebib-autoloads.el"
;;;;;;  "../../../../../.emacs.d/elpa/ebib-20160604.53/ebib-db.el"
;;;;;;  "../../../../../.emacs.d/elpa/ebib-20160604.53/ebib-filters.el"
;;;;;;  "../../../../../.emacs.d/elpa/ebib-20160604.53/ebib-keywords.el"
;;;;;;  "../../../../../.emacs.d/elpa/ebib-20160604.53/ebib-pkg.el"
;;;;;;  "../../../../../.emacs.d/elpa/ebib-20160604.53/ebib-utils.el"
;;;;;;  "../../../../../.emacs.d/elpa/ebib-20160604.53/ebib.el")
;;;;;;  (22361 45733 202000 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ebib-autoloads.el ends here
