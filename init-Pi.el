;; following Systems Crafters config
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))
(add-hook 'emacs-startup-hook
	  ;; Profile emacs startup
	  (lambda ()
	    (message "*** Emacs loaded in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)


;; APPEARANCE

;; MISC
(use-package emacs
  :demand t
  :ensure nil
  :init
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq bookmark-save-flag 1)
  (setq inhibit-startup-message t)
  (setq make-backup-files nil)
  (auto-save-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (menu-bar-mode -1)
  (tab-bar-history-mode 1)
  (setq ring-bell-function 'ignore)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  )

(setq org-directory (concat (getenv "HOME") "/org-setup/org")
      org-journal (concat org-directory "/journal")
      org-notes (concat org-directory "/ZK")
      zot-bib (concat (getenv "HOME") "/Documents/zotLib.bib")
      org-roam-directory org-notes)

(use-package org
  :ensure t
  :demand t
  :init
  (setq org-auto-align-tags nil
        org-tags-column 0)

  ;; todo setup
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d)" "CANCELLED")
	  (sequence "PROJ" "|" "COMPLETED")))
  (setq org-adapt-indentation nil)   ;; interacts poorly with 'evil-open-below'
  (setq org-image-actual-width 200)

  :config
  ;; annoying problem where org-journal breaks if I don't remove trailing whitespace
  ;; doom does this automatically not clear where
  (defun my-org-mode-setup ()
    "Custom configurations for `org-mode`."
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))
  (add-hook 'org-mode-hook 'my-org-mode-setup))

(use-package org-journal
  :ensure t
  :config
  (setq org-journal-dir (concat org-directory "/journal")
	org-journal-file-type 'monthly
	org-journal-file-format "%Y-%m.org"
	org-journal-enable-agenda-integration nil    ;; TODO change this at some point
	org-extend-today-until 4
	org-journal-date-format "%a, %Y-%m-%d"
	;; org-journal-enable-encryption t
	org-journal-find-file #'find-file)

  (defun baz/org-journal-new-diary-entry ()
    (call-interactively 'org-journal-new-entry)
    (org-set-tags "diary"))

  (defun baz/org-journal-new-entry-with-tags ()
    (call-interactively 'org-journal-new-entry)
    (org-set-tags "diary")))


(load-file (expand-file-name
 	    "journal-config.el" user-emacs-directory))
