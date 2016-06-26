;;;;----------------------------------------------------------------------------
;;;; Functions for creating blog posts with Jekyll.
;;;;----------------------------------------------------------------------------

(setq blog-base-dir "/home/bryan/Projects/personal-site/")
(setq blog-posts-dir (concat blog-base-dir "_posts/"))

(defun new-blog-post (title)
  "Prompt for a blog post title, then create the file with the
proper pre-amble."
  (interactive "sTitle: ")
  (let ((file-name (title-to-file-name title))
	(todays-date (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))
    (find-file (concat blog-posts-dir todays-date "-" file-name))

    (insert "---")
    (newline)
    (insert "layout: post")
    (newline)
    (insert (concat "title: " title))
    (newline)
    (insert "---")

    (newline)
    (newline)))

;;;;----------------------------------------------------------------------------

(defun title-to-file-name (title)
  (concat
   (downcase
    (replace-regexp-in-string
     "[[:space:]]" "-"
     (replace-regexp-in-string "[^[:alnum:][:space:]]" "" title)))
   ".md"))

;;;;----------------------------------------------------------------------------

(provide 'bst-blogging)
