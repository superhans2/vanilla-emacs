;; UTILITY FUNCTIONS:
;; default starts looking in documents since this is where I store notes
(setq default-directory "c:/Users/HughesDavA/Documents/")

    
(setq inhibit-splash-screen t)
(require 'bookmark)
(list-bookmarks)
(switch-to-buffer "*Bookmark List*")
;; save bookmark on change
(setq bookmark-save-flag 1)

;; TODO make this org-directory
;; "c:/Users/HughesDavA/Documents/org/"

;; modifying for windows
;; unpicking journal:
(setq org-journal-file-type 'yearly
      org-journal-find-file #'find-file)

(setq magit-git-executable "c:/Program Files/Git/bin/git")
