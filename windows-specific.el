;; UTILITY FUNCTIONS:
;; default starts looking in documents since this is where I store notes
(setq default-directory "c:/Users/HughesDavA/Documents")
(setq magit-git-executable "c:/Program Files/Git/bin/git")
(require 'bookmark)
;; (list-bookmarks)
;; (switch-to-buffer "*Bookmark List*")
(setq bookmark-save-flag 1)



(setq org-directory (concat default-directory "/org")
      org-notes (concat org-directory "/ZK")
      ;;zot-bib (concat (getenv "HOME") "/Documents/zotLib.bib")
      org-roam-directory org-notes)

(use-package org-journal
  :config
  (setq org-journal-file-type 'yearly
	org-journal-find-file #'find-file))

(defun baz/tab-bar-run-config ()
  (interactive)
  (mmk2410/tab-bar-switch-or-create
   "Config"
   (lambda ()
     (find-file (expand-file-name "init.el" user-emacs-directory)))))

(defun baz/startup ()
  (progn
    ;; setting up Org tab
    (tab-rename "Org")
    (org-journal-open-current-journal-file)))
