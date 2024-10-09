;; TODO serious refactoring

;; code for accommodating now-extend-time
(defun baz/it-is-in-extended-zone ()
  "In the time after midnight but before I count the next day having begun."
    (< (nth 2 (decode-time)) org-extend-today-until))

;; accessing location to reformat to
(defun baz/current-day (now)
  (if (baz/it-is-in-extended-zone)
      (format-time-string org-journal-date-format (encode-time (nth 0 now)
                        (nth 1 now)
                        (nth 2 now)
                        (1- (nth 3 now))
                        (nth 4 now)
                        (nth 5 now)
                        (nth 8 now)))
      (format-time-string org-journal-date-format (current-time))))

(baz/current-day (decode-time nil))   ;; TODO need to work out how to assign variable within emacs function


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
     (baz/current-day (decode-time nil)))
    (switch-to-buffer (other-buffer))))
