;; UTILITY FUNCTIONS:
;; default starts looking in documents since this is where I store notes
(setq default-directory "c:/Users/HughesDavA/Documents/")

;; BOOK MARK system
(setq inhibit-splash-screen t)
(require 'bookmark)
(list-bookmarks)
(switch-to-buffer "*Bookmark List*")
;; save bookmark on change
(setq bookmark-save-flag 1)


