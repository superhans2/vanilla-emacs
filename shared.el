;;; shared.el -*- lexical-binding: t; -*-

;; shared config between vanill and doom

(define-key global-map (kbd "<f5>") #'modus-themes-toggle)


(use-package olivetti
  :init
  (setq olivetti-body-width 90)
  (setq olivetti-style 'fancy)
  (setq olivetti-minimum-body-width 50))

;;(add-hook 'olivetti-mode-on-hook (lambda () (olivetti-set-width body-width-default)))
;;(add-hook! 'org-mode-hook #'olivetti-mode)

;;
;; shared org config
(use-package org
  ;;  :demand t
  :init
  (setq org-directory (concat (getenv "HOME") "/shared/org")
        org-notes (concat org-directory "/ZK")
        zot-bib (concat (getenv "HOME") "/Documents/zotLib.bib"))
  (setq org-auto-align-tags nil
        org-tags-column 0
        org-startup-folded "fold")
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d)" "CANCELLED")
          (sequence "PROJ" "|" "COMPLETED")))
  (setq org-adapt-indentation nil)   ;; interacts poorly with 'evil-open-below'
  (setq org-todo-keyword-faces
        '(("IDEA" . (:foreground "GoldenRod" :weight bold))
          ("NEXT" . (:foreground "IndianRed1" :weight bold))
          ("WAIT" . (:foreground "coral" :weight bold))
          ("CANCELLED" . (:foreground "blue" :weight bold))
          ("SOMEDAY" . (:foreground "LimeGreen" :weight bold))))

  (setq org-agenda-files
        (list
         (concat org-directory "/journal/")
         (concat org-directory "/scrap.org")
         (concat org-directory "/projects/")
         (concat org-directory "/inbox.org")))
  (setq org-tag-alist '(
                        ;; ticket types
                        ("kindling")
                        ("recipe")
                        ("diary")
                        ("crypt")
                        ("emacs")
                        ("van")
                        ("therapy")
                        ("poem")
                        ("music")
                        ("makeup")
                        ("linux")
                        ("phd")
                        ("fix")
                        ("clothing")
                        ("password")
                        ("tech")
                        ("therapy")
                        ("bee")
                        ))


  ;; TODO fix
  ;; :general
  ;; (baz/local-leader-keys
  ;;   :keymaps 'org-mode-map
  ;;   "a" '(org-archive-subtree :wk "archive")
  ;;   "d" '(org-decrypt-entry :wk "decrypt org entry")
  ;;   "t" '(org-todo :wk "todo")
  ;;   "s" '(org-insert-structure-template :wk "template")
  ;;   "e" '(org-edit-special :wk "edit")
  ;;   ">" '(org-demote-subtree :wk "demote subtree")
  ;;   "<" '(org-promote-subtree :wk "demote subtree"))

  :hook
  (org-mode . olivetti-mode)
  (org-mode . variable-pitch-mode)
  :config

  (setq org-agenda-custom-commands
        '(("n" "TODOs sorted by priority (with priority only)"
           ((todo "TODO" ;; "+TODO=\"TODO\"|TODO=\"WAIT\""
           ((org-agenda-sorting-strategy '(priority-down))
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if 'notregexp "#."))))
            (todo "WAIT")
            (todo "TODO"
           ((org-agenda-skip-function
             '(org-agenda-skip-entry-if 'regexp "#.")))))
           )))

  ;; annoying problem where org-journal breaks if I don't remove trailing whitespace
  ;; doom does this automatically not clear where
  (defun my-org-mode-setup ()
    "Custom configurations for `org-mode`."
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))
  (add-hook 'org-mode-hook 'my-org-mode-setup)

  ;;(add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))
  (setq-default org-startup-indented t
                org-pretty-entities t
                ;; org-fontify-quote-and-verse-blocks t
                ;; org-use-sub-superscripts "{}"
                org-hide-emphasis-markers t
                org-startup-with-inline-images t
                org-image-actual-width '(300)))

;; org-journal
(use-package org-journal
  :init
  (setq
      org-journal-dir (concat org-directory "/journal")
      org-journal-file-type 'monthly
      org-journal-file-format "%Y-%m.org"
      org-journal-time-format "%R "
      org-journal-carryover-items ""
      org-journal-enable-agenda-integration nil
      org-extend-today-until 4
      org-journal-date-format "%a, %Y-%m-%d"
      org-journal-find-file #'find-file)
  )
