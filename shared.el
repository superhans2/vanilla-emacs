;;; shared.el -*- lexical-binding: t; -*-

;;; helper config
(defmacro baz/use-package (name &rest args)
  "Shared `use-package` wrapper.
- On Doom: expands to `(after! NAME (use-package! NAME ...))`
- On vanilla: expands to `(use-package NAME ...)`"
  (if (featurep 'doom)
      ;; Doom: delay config until after Doom’s own
      `(after! ,name
         (use-package! ,name ,@args))
    ;; Vanilla: normal use-package
    `(use-package ,name ,@args)))


;;; appearance
(setq modus-themes-to-toggle '(modus-vivendi-tinted modus-operandi-tinted))
(define-key global-map (kbd "<f5>") #'modus-themes-toggle)

;;; Olivetti
(use-package olivetti
  :init
  (setq olivetti-body-width 90)
  (setq olivetti-style 'fancy)
  (setq olivetti-minimum-body-width 50))


;;; Org

;; shared variables
(setq org-directory (concat (getenv "HOME") "/org")
      org-notes org-directory ;;(concat org-directory "/ZK")
      zot-bib (concat (getenv "HOME") "/Documents/zotLib.bib"))

(baz/use-package org
  ;;  :demand t
  :init
  (setq org-auto-align-tags nil
        org-tags-column 0
        org-startup-folded "fold")
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELLED")
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
         (concat org-directory "/projects/")
         (concat org-directory "/inbox.org")))
  (setq org-tag-alist '(
                        ;; ticket types
                        ("kindling") ("recipe") ("diary") ("crypt") ("emacs") ("van") ("therapy")
                        ("poem") ("music") ("makeup") ("linux") ("phd") ("fix") ("clothing")
                        ("password") ("tech") ("therapy") ("bee")
                        ))


  :hook
  (org-mode . olivetti-mode)

  :general
  (:keymaps 'org-mode-map
   :prefix "C-c"
   "d" #'org-decrypt-entry)

  :config
  (setq org-attach-dir-relative t
        org-attach-store-link-p 'file
        org-attach-id-dir "data/"
        org-yank-dnd-method 'attach
        org-attach-id-dir "data/"
        org-attach-use-inheritance t)

  (setq org-agenda-custom-commands
        '(
	  ("n" "TODOs sorted by priority (with priority only)"
           ((todo "TODO" ;; "+TODO=\"TODO\"|TODO=\"WAIT\""
           ((org-agenda-sorting-strategy '(priority-down))
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if 'notregexp "#."))))
            (todo "WAIT")
            (todo "TODO"
           ((org-agenda-skip-function
             '(org-agenda-skip-entry-if 'regexp "#.")))))
           )
	  ))

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

;;;; org-journal
(baz/use-package org-journal
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
        org-journal-find-file #'find-file))

;;;; org-crypt
(baz/use-package org-crypt
  :after org
  :config
  (org-crypt-use-before-save-magic)
  (setq org-crypt-tag-matcher "crypt")
  (setq org-crypt-key "C0FC1B41A828E1FA")
  (setq auto-save-default nil))

;;;; org-roam
(baz/use-package org-roam
  :custom
  (org-roam-directory org-notes)

  :bind
  (("C-c n l" . org-roam-buffer-toggle)
  ("C-c n g" . org-roam-graph)
  ("C-c f" . org-roam-node-find)
  ("C-c i" . org-roam-node-insert)
  ("C-c n c" . org-roam-capture))

  :config
  (org-roam-db-autosync-mode)
  (add-to-list 'display-buffer-alist
              '("\\*org-roam\\*"
              (display-buffer-in-direction)
              (direction . right)
              (window-width . 0.33)
              (window-height . fit-window-to-buffer)))

  ;; fixes issue where roam splits window
  (defun +org-roam-reuse-windows (&rest r)
    (when org-roam-buffer-current-node
      (let ((window (get-buffer-window
                  (get-file-buffer
                      (org-roam-node-file org-roam-buffer-current-node)))))
      (when window (select-window window)))))
  (advice-add 'org-roam-preview-visit :before #'+org-roam-reuse-windows)
  (advice-add 'org-roam-node-visit :before #'+org-roam-reuse-windows)
)

;;;; agenda refiler

;;; supersave
(use-package super-save
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t)
  (add-to-list 'super-save-predicates (lambda ()
                                        (if (buffer-file-name)
                                            (string-match-p "org" (buffer-file-name))
                                          nil))))


;;; open with
(use-package openwith
  :config
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("pdf"))
               "okular"
               '(file))
         ))
  :init
  (openwith-mode 1))
