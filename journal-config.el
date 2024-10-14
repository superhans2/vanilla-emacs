;; TODO serious refactoring

;; code for accommodating now-extend-time
(defun baz/it-is-in-extended-zone ()
  "In the time after midnight but before I count the next day having begun."
  (< (nth 2 (decode-time)) org-extend-today-until))

;; accessing location to reformat to
(defun baz/current-day ()
  (let (now (decode-time nil))
    (if '() ;; (baz/it-is-in-extended-zone)
	;; this bit doesn't work
	;; TODO fix
	(format-time-string org-journal-date-format (encode-time (nth 0 now)
								 (nth 1 now)
								 (nth 2 now)
								 (1- (nth 3 now))
								 (nth 4 now)
								 (nth 5 now)
								 (nth 8 now)))
      (format-time-string org-journal-date-format (current-time)))))

(defun baz/refile (file headline)
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile nil nil (list headline file nil pos))))

(defun baz/refile-journal ()
  (interactive)
  (setq org-log-refile nil)
  (setq org-reverse-note-order t)
  (progn
    (org-todo "DONE")
    (baz/refile
     (org-journal--get-entry-path)
     (baz/current-day))
    (switch-to-buffer (other-buffer))))


;; AUTOMATICALLY LOADING IMAGES INTO JOURNAL:  

(setq tmp-directory "~/code/elisp/tmp-files/"
      dest-directory "~/code/elisp/files/") 

(defun baz/insert-images-into-journal ()
  (dolist (old-file-path (directory-files tmp-directory t directory-files-no-dot-files-regexp))
    (let* ((old-file-path-no-dir (file-name-nondirectory old-file-path))
	   (new-filename (concat dest-directory old-file-path-no-dir)))

      ;; move file 
      (rename-file old-file-path new-filename)

      ;; create a link to new file in journal
      (baz/create-link-to-file new-filename))
    ))

(defun baz/org-capture-image (list-of-image-paths)
  (let* ((content-text "testing.png")
	 (org-capture-templates `(("ji" "journal entry" entry (file+headline ,(org-journal--get-entry-path) ,(baz/current-day))
				   ,(concat "* TODO image \n" content-text)
				   :immediate-finish t))))
    ;; Use `org-capture' with a specific template without user input
    (org-capture nil "ji")))




(defun baz/create-link-to-file (filepath)
  (message "creating link to %s" filepath)
  (call-interactively 'org-journal-new-entry)
  (insert (format "\n[[file:%s]]" filepath))
  (save-buffer)
  (kill-buffer))

(baz/insert-images-into-journal)
