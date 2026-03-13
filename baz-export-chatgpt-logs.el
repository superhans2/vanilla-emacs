;;; baz-export-chatgpt-logs.el -*- lexical-binding: t; -*-
;;; Library for exporting chatgpt logs
;;; (see entry in journal "how to backup chatgpt logs" for further context) 


(defun baz/extract-date ()
  ;; get the second occurrence of date in format 2026-xx-xx
  ;; this should be the last time the chatgpt buffer was update
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" nil t 2)
       (match-string-no-properties 0))))

(defun baz/get-journal-file-path (date)
  "Gets the journal file name"
  (let* ((truncated_date (substring date 0 7))
         (journal_file_name (concat truncated_date ".org"))
         (journal_file_path (expand-file-name journal_file_name org-journal-dir)))
    journal_file_path))

;; (baz/get-top-level-heading "~/org/journal/2026-04.org" "2026-04-01")

(defun baz/get-top-level-heading-pos (journal-file-path date)
  "Gets the position of the top level heading with a matching regexp"
  (with-current-buffer (find-file-noselect journal-file-path)
    (goto-char (point-min))  ;; makes sure at the beginning of the buffer
    (let (results)
      (while (re-search-forward date nil t)
        (when (and (= (org-outline-level) 1) (org-at-heading-p))
          (push (cons (org-get-heading t t t t)
                      (line-beginning-position))
                results)))

      ;; TODO would be nice if it flags error if this is empty list or if list is greater than 1
      (cdr (car results))
      )    

    )
  )


(defun baz/get-top-level-heading-for-day (journal-file-path date)
  "Given the journal-file-path, visit this file, get the ")

(defun baz/refile-to-target ()
  "
- assume at the top level of the org file
- give the chatgpt tag
- get the date
- get the top level subheading for the date
- 
"
  (let* ((date (baz/extract-date))
         (journal-file-path (baz/get-journal-file-path date))
         (top-level-heading-pos (baz/get-top-level-heading-pos journal-file-path date)))

    (org-refile nil nil (list "date" journal-file-path nil top-level-heading-pos))
    )
  )


(defun baz/convert-md-to-org-then-refile ()
  "
From within a chatgpt markdown file this will convert to org,
go to the top of the file, add a tag and then refile to the correct location
"
  (call-interactively 'org-pandoc-import-to-org)
  (sleep-for 1)
  (let ((org-file-path (concat (substring (buffer-file-name) 0 -3) ".org")))
    (message org-file-path)
    (find-file org-file-path)
    (goto-char (point-min))
    (org-toggle-tag "chatgpt" 'on)
    (baz/refile-to-target)
    )
  )

(provide 'baz-export-chatgpt-logs)
;;; baz-export-chatgpt-logs.el ends here
