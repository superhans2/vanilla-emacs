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

(defvar tmp-directory "~/code/elisp/tmp-files/")
(defvar dest-directory "~/code/elisp/files/")

(defun baz/insert-images-into-journal ()
  (dolist
      (file-path (baz/filter-list-of-stfolder (directory-files tmp-directory t  directory-files-no-dot-files-regexp)))
    (if (file-directory-p file-path)
	(baz/handle-directory-of-images file-path)
      (baz/handle-single-image file-path))
    )
  )

(defun baz/filter-list-of-stfolder (filelist)
  (seq-remove (lambda (x) (string-match "c.txt" (file-name-nondirectory x))) filelist)
  )



(defun baz/handle-single-image (image-filename)
  """ Handles single images moving into journal
"""
    (let* ((old-file-path-no-dir (file-name-nondirectory image-filename))
	   (new-filename (concat dest-directory old-file-path-no-dir)))

      ;; move file
      (rename-file image-filename new-filename)

      ;; create a link to new file in journal
      (baz/org-capture-image (list new-filename))
      )
  )

(defun baz/handle-directory-of-images (image-directory-filename)
  """ Handles directory of images moving into journal"
  (let* ((filenames (directory-files image-directory-filename t directory-files-no-dot-files-regexp)))
    (if filenames
	(baz/org-capture-image (baz/move-list-and-return-changed-filenames filenames)))
  ))

(defun baz/move-list-and-return-changed-filenames (list-of-filenames)
  """helper method for baz/handle-directory-of-images"""
  (let* ((old-filename (car list-of-filenames))
	 (old-file-path-no-dir (file-name-nondirectory old-filename))
	 (new-filename (concat dest-directory old-file-path-no-dir)))
    ;; move the file
    (rename-file old-filename new-filename)

    ;; move entire list 
    (if (cdr list-of-filenames)
	(cons new-filename (baz/move-list-and-return-changed-filenames (cdr list-of-filenames)))
      (list new-filename))
    )
  )


(defun baz/org-capture-image (list-of-image-paths)
  (let* ((content-text (baz/generate-org-content-from-image-paths list-of-image-paths))
	 (org-capture-templates `(("ji" "journal entry" entry (file+headline ,(org-journal--get-entry-path) ,(baz/current-day))
				   ,(concat "* TODO image" content-text)
				   :immediate-finish t))))
    ;; Use `org-capture' with a specific template without user input
    (org-capture nil "ji")))

(defun baz/generate-org-content-from-image-paths (list-of-image-paths)
  (message (car list-of-image-paths))
  (if list-of-image-paths
      (concat "\n" "[[" (car list-of-image-paths) "]]" (baz/generate-org-content-from-image-paths (cdr list-of-image-paths)))
    ""
    )
)




(defun baz/create-link-to-file (filepath)
  (message "creating link to %s" filepath)
  (call-interactively 'org-journal-new-entry)
  (insert (format "\n[[file:%s]]" filepath))
  (save-buffer)
  (kill-buffer))

(baz/insert-images-into-journal)
