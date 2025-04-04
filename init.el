;; -*- outline-blank-line: t; lexical-binding: t; -*-
;;; INITIAL
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
(setq use-package-always-ensure t)

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
;(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering
  :config


  ;; no-littering doesn't set this by default so we must place
  ;; auto save files in the same path as it uses for sessions
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  )


;;; APPEARANCE
;;;; font
(defvar baz/default-font-size 160)
(set-face-attribute 'default nil :font "JetBrains Mono" :height baz/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height baz/default-font-size)
(set-face-attribute 'variable-pitch nil :font "JetBrains Mono" :height baz/default-font-size :weight 'regular)
(load-theme 'modus-vivendi)
(define-key global-map (kbd "<f5>") #'modus-themes-toggle)

;;;; olivetti
(use-package olivetti
  :demand t
  :init
  (setq olivetti-body-width 90)
  (setq olivetti-style 'fancy)
  (setq olivetti-minimum-body-width 50))

(setq-default line-spacing 4)


;;; EMACS CONFIG 

(use-package emacs
  :demand t
  :ensure nil
  :init

  ;; using specific custom file
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file))

  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq bookmark-save-flag 1)
  (setq inhibit-startup-message t)
  (setq make-backup-files nil)
  (cua-mode)
  (auto-save-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (menu-bar-mode -1)
  (tab-bar-history-mode 1)
  (setq ring-bell-function 'ignore)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))


;;; KEYBINDING 
;;;; packages
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))
(use-package hydra)

;;;; evil
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll nil)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection 
  :after evil
  :init
  ;; If I want to incrementally enable evil-collection mode-by-mode, I can do something like the following:
  ;; (setq evil-collection-mode-list nil) ;; I don't like surprises
  ;; (add-to-list 'evil-collection-mode-list 'magit) ;; evilify magit
  (setq evil-collection-setup-minibuffer t)

  :config
  (global-set-key (kbd "C-z") 'evil-exit-emacs-state)
  (evil-collection-init))

;; (general-define-key
;;  :states '(normal)
;;  :keymaps 'org-mode-map
;;  "RET" #'+org/dwim-at-point)

;;;; bindings
(use-package general
  :ensure t
  :demand t
  :config
  (general-evil-setup)
  ;; integrate general with evil

  ;; set up 'SPC' as the global leader key
  (general-create-definer baz/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  ;; set up ',' as the local leader key
  (general-create-definer baz/local-leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "," ;; set local leader
    :global-prefix "M-,") ;; access local leader in insert mode

  ;; unbind some annoying default bindings
  (general-unbind
    "<mouse-2>") ;; pasting with mouse wheel click

  (baz/leader-keys
    "SPC" '(execute-extended-command :wk "execute command") ;; an alternative to 'M-x'
    "TAB" '(:keymap tab-prefix-map :wk "tab")) ;; remap tab bindings

  (baz/leader-keys
    "w" '(:keymap evil-window-map :wk "window")
    "wu" '(tab-bar-history-back :wk "window arrangement undo")
    "wU" '(tab-bar-history-forward :wk "window arrangement redo")) ;; window bindings

  (baz/leader-keys
    "s" '(:ignore t :wk "search"))

  (baz/leader-keys
    "c" '(:ignore t :wk "code"))


  (baz/local-leader-keys
    :keymaps 'emacs-lisp-mode-map
    "h" '(hs-hide-all :wk "hide all")
    "," '(hs-toggle-hiding :wk "toggle code block")
    "o" '(hs-show-block :wk "show block")
    "O" '(hs-hide-block :wk "hide block"))

  ;; help
  ;; namespace mostly used by 'helpful'
  (baz/leader-keys
    "h" '(tab-previous :wk "tab previous"))
  (baz/leader-keys
    "l" '(tab-next :wk "tab next"))

  (baz/leader-keys
    "h" '(:ignore t :wk "help"))

  ;; file
  (baz/leader-keys
    "f" '(:ignore t :wk "file")
    "ff" '(consult-file :wk "find file") ;; gets overridden by consult
    "fs" '(save-buffer :wk "save file"))

  ;; buffer
  ;; see 'bufler' and 'popper'
  (baz/leader-keys
    "b" '(:ignore t :wk "buffer")
    "bb" '(consult-buffer :wk "switch buffer") ;; gets overridden by consult
    "bk" '(kill-this-buffer :wk "kill this buffer")
    "bi" '(ibuffer :wk "ibuffer")
    "br" '(revert-buffer :wk "reload buffer"))

  ;; bookmark
  (baz/leader-keys
    "B" '(:ignore t :wk "bookmark")
    "Bs" '(bookmark-set :wk "set bookmark")
    "Bj" '(bookmark-jump :wk "jump to bookmark"))

  ;; universal argument
  (baz/leader-keys
    "u" '(universal-argument :wk "universal prefix"))

  ;; notes
  ;; see 'citar' and 'org-roam'
  (baz/leader-keys
    "n" '(:ignore t :wk "notes")
    ;; see org-roam and citar sections
    "na" '(org-todo-list :wk "agenda todos")) ;; agenda

  ;; code
  ;; see 'flymake'
  (baz/leader-keys
    "c" '(:ignore t :wk "code"))

  ;; org capture
  (baz/leader-keys
    "x" '(org-capture :wk "capture"))

  (baz/leader-keys
    "." '(find-file :wk "switch buffer"))

  ;; buffer list
  (baz/leader-keys
    "," '(consult-buffer :wk "switch buffer"))

  ;; open
  (baz/leader-keys
    "o" '(:ignore t :wk "open")
    "o-" '(dired-jump :wk "open in dired")
    "oa" '(org-agenda :wk "org agenda")
    "os" '(speedbar t :wk "speedbar")) ;; TODO this needs some love

  ;; toggle
  (baz/leader-keys
    "t" '(:ignore t :wk "toggle")
    "tt" '(tab-bar-mode :wk "toggle tab bar mode")
    "td" '(baz/toggle-dired-details :wk "toggle details in dired")
    "tv" '(visual-line-mode :wk "visual line mode")
    "to" '(olivetti-mode :wk "toggle olivetti mode")) 

  ;; search
  (baz/leader-keys
    "s"  '(:ignore t :wk "search")
    "sd" '(consult-grep :wk "search directory")
    "ss" '(consult-line :wk "search line")
    "sG" '(consult-git-grep :wk "consult git grep"))

  ;; templating
  (baz/leader-keys
    "t" '(:ignore t :wk "template")))

(global-set-key (kbd "M-2") 'tab-next)
(global-set-key (kbd "M-1") 'tab-previous)

;; (defhydra hydra-text-scale (:timeout 4)
;;   "scale text"
;;   ("j" text-scale-increase "in")
;;   ("k" text-scale-decrease "out"))

;;; COMPLETION FRAMEWORK
(use-package vertico
  :ensure t
  :bind (:map vertico-map
	      ("C-j" . vertico-next)
	      ("C-k" . vertico-previous)
	      ("C-f" . vertico-exit)
	      :map minibuffer-local-map
	      ("M-h" . backward-kill-word))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package corfu
  :custom
  (corfu-auto t)
  :hook
  (prog-mode . corfu-mode))

(use-package savehist
  ;; Persist history over Emacs restarts. Vertico sorts by history position.
  :init
  (savehist-mode))

(use-package consult
  :demand t
  :ensure t)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))


;;; NAVIGATION


;;;; opening bookmark as tab
;; Add custom keybindings within the tab-prefix-map
(define-key tab-prefix-map (kbd "n") 'baz/open-new-tab)
(define-key tab-prefix-map (kbd "2") 'tab-duplicate)
(define-key tab-prefix-map (kbd "j") 'mmk2410/tab-bar/body)
(defun baz/open-new-tab ()
  (interactive)
  (progn
    (tab-new)
    (scratch-buffer)
    (call-interactively 'bookmark-bmenu-list)))   

(setq tab-bar-new-tab-choice #'get-scratch-buffer-create)
(defun baz/load-bookmarks-after-new-tab (&rest _args)
  (interactive)
  (call-interactively 'bookmark-bmenu-list))
(advice-add 'tab-bar-new-tab :after #'baz/load-bookmarks-after-new-tab)

(winner-mode)
(tab-bar-mode)  ;; TAB BAR MODE on by default 


;;; ESSENTIAL TOOLS 
;;;; dired
(defvar dired-details-enabled t)

(defun baz/toggle-dired-details()
  (if dired-details-enabled
      (setq dired-details-enabled '())
    (setq dired-details-enabled t)))
(add-hook 'dired-mode-hook
	  (lambda ()
	    (dired-hide-details-mode)))


;;;; magit
(use-package magit
  :ensure t
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :general
  (baz/leader-keys
    "g" '(:ignore t :wk "git")
    "gg" '(magit-status :wk "magit status")))


;;;; snippets
(use-package yasnippet
  :hook
  (org-mode . yas-minor-mode))

(use-package doom-snippets
  :load-path "~/vanilla-emacs/local-packages/snippets"
  :after yasnippet
  :config
  ;; FIXME why does linter complain that this might not be defined at runtime?
  (yas-reload-all))


;;; ORG 
;;;; general 

(use-package org
  :ensure t
  :demand t
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
  (setq org-agenda-files
	(list 
         (concat org-directory "/journal/")
	 (concat org-directory "/scrap.org")
	 (concat org-directory "/projects/")
         (concat org-directory "/inbox.org")))


  :general
  (baz/local-leader-keys
    :keymaps 'org-mode-map
    "a" '(org-archive-subtree :wk "archive")
    "d" '(org-decrypt-entry :wk "decrypt org entry")
    "t" '(org-todo :wk "todo")
    "s" '(org-insert-structure-template :wk "template")
    "e" '(org-edit-special :wk "edit")
    ">" '(org-demote-subtree :wk "demote subtree")
    "<" '(org-promote-subtree :wk "demote subtree"))

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

;;;; dwim
(defun +org/dwim-at-point (&optional arg)
    "Do-what-I-mean at point.

If on a:
- checkbox list item or todo heading: toggle it.
- citation: follow it
- headline: cycle ARCHIVE subtrees, toggle latex fragments and inline images in
  subtree; update statistics cookies/checkboxes and ToCs.
- clock: update its time.
- footnote reference: jump to the footnote's definition
- footnote definition: jump to the first reference of this footnote
- timestamp: open an agenda view for the time-stamp date/range at point.
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: execute the source block
- statistics-cookie: update it.
- src block: execute it
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
    (interactive "P")
    (if (button-at (point))
	(call-interactively #'push-button)
      (let* ((context (org-element-context))
             (type (org-element-type context)))
	;; skip over unimportant contexts
	(while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
          (setq context (org-element-property :parent context)
		type (org-element-type context)))
	(pcase type
          ((or `citation `citation-reference)
           (org-cite-follow context arg))

          (`headline
           (cond ((memq (bound-and-true-p org-goto-map)
			(current-active-maps))
                  (org-goto-ret))
		 ((and (fboundp 'toc-org-insert-toc)
                       (member "TOC" (org-get-tags)))
                  (toc-org-insert-toc)
                  (message "Updating table of contents"))
		 ((string= "ARCHIVE" (car-safe (org-get-tags)))
                  (org-force-cycle-archived))
		 ((or (org-element-property :todo-type context)
                      (org-element-property :scheduled context))
                  (org-todo
                   (if (eq (org-element-property :todo-type context) 'done)
                       (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
                           'todo)
                     'done))))
           ;; Update any metadata or inline previews in this subtree
           (org-update-checkbox-count)
           (org-update-parent-todo-statistics)
           (when (and (fboundp 'toc-org-insert-toc)
                      (member "TOC" (org-get-tags)))
             (toc-org-insert-toc)
             (message "Updating table of contents"))
           (let* ((beg (if (org-before-first-heading-p)
                           (line-beginning-position)
			 (save-excursion (org-back-to-heading) (point))))
                  (end (if (org-before-first-heading-p)
                           (line-end-position)
			 (save-excursion (org-end-of-subtree) (point))))
                  (overlays (ignore-errors (overlays-in beg end)))
                  (latex-overlays
                   (cl-find-if (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
                               overlays))
                  (image-overlays
                   (cl-find-if (lambda (o) (overlay-get o 'org-image-overlay))
                               overlays)))
             (+org--toggle-inline-images-in-subtree beg end)
             (if (or image-overlays latex-overlays)
		 (org-clear-latex-preview beg end)
               (org--latex-preview-region beg end))))

          (`clock (org-clock-update-time-maybe))

          (`footnote-reference
           (org-footnote-goto-definition (org-element-property :label context)))

          (`footnote-definition
           (org-footnote-goto-previous-reference (org-element-property :label context)))

          ((or `planning `timestamp)
           (org-follow-timestamp-link))

          ((or `table `table-row)
           (if (org-at-TBLFM-p)
               (org-table-calc-current-TBLFM)
             (ignore-errors
               (save-excursion
		 (goto-char (org-element-property :contents-begin context))
		 (org-call-with-arg 'org-table-recalculate (or arg t))))))

          (`table-cell
           (org-table-blank-field)
           (org-table-recalculate arg)
           (when (and (string-empty-p (string-trim (org-table-get-field)))
                      (bound-and-true-p evil-local-mode))
             (evil-change-state 'insert)))

          (`babel-call
           (org-babel-lob-execute-maybe))

          (`statistics-cookie
           (save-excursion (org-update-statistics-cookies arg)))

          ((or `src-block `inline-src-block)
           (org-babel-execute-src-block arg))

          ((or `latex-fragment `latex-environment)
           (org-latex-preview arg))

          (`link
           (let* ((lineage (org-element-lineage context '(link) t))
                  (path (org-element-property :path lineage)))
             (if (or (equal (org-element-property :type lineage) "img")
                     (and path (image-type-from-file-name path)))
		 (+org--toggle-inline-images-in-subtree
                  (org-element-property :begin lineage)
                  (org-element-property :end lineage))
               (org-open-at-point arg))))

          ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
           (org-toggle-checkbox))

          (`paragraph
           (+org--toggle-inline-images-in-subtree))

          (_
           (if (or (org-in-regexp org-ts-regexp-both nil t)
                   (org-in-regexp org-tsr-regexp-both nil  t)
                   (org-in-regexp org-link-any-re nil t))
               (call-interactively #'org-open-at-point)
             (+org--toggle-inline-images-in-subtree
              (org-element-property :begin context)
              (org-element-property :end context))))))))

(general-define-key
 :states '(normal)
 :keymaps 'org-mode-map
 "RET" #'+org/dwim-at-point)


;;;; beautifying org-mode
  ;; use org-bullets
  (require 'org-bullets)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

  ;;
  ;; Increase line spacing
  ;; (setq line-spacing 2)


  ;;(add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))
  (setq-default org-startup-indented t
		org-pretty-entities t
		;; org-fontify-quote-and-verse-blocks t
		;; org-use-sub-superscripts "{}"
		org-hide-emphasis-markers t
		org-startup-with-inline-images t
		org-image-actual-width '(300)))

(use-package org-bullets)

  

;;;; org journal

(use-package org-journal
  :config
  (setq org-journal-dir (concat org-directory "/journal")
	org-journal-file-type 'monthly
	org-journal-file-format "%Y-%m.org"
	org-journal-enable-agenda-integration nil    ;; TODO change this at some point
	org-extend-today-until 4
	org-journal-carryover-items ""
	org-journal-date-format "%a, %Y-%m-%d"
	;; org-journal-enable-encryption t
	org-journal-find-file #'find-file)

  (defun baz/org-journal-new-diary-entry ()
    (interactive)
    (call-interactively 'org-journal-new-entry)
    (org-set-tags "diary"))

  (defun baz/org-journal-new-entry-with-tags ()
    (call-interactively 'org-journal-new-entry)
    (org-set-tags "diary"))

  (defun baz/org-journal-narrow-today ()
    (interactive)
    (let ((current-prefix-arg '(4)))  ;; Simulate C-u
      (call-interactively 'org-journal-new-entry))
      (call-interactively 'outline-up-heading)
    (call-interactively 'org-narrow-to-subtree)
    )
  ;; (baz/org-journal-narrow-today)


  :general
  (baz/leader-keys
    "nj" '(org-journal-new-entry :wk "create new entry")
    "ng" '(org-journal-open-current-journal-file :wk "go to current journal file")
    "nn" '(baz/org-journal-narrow-today :wk "go to current journal file")
    "nd" '(baz/org-journal-new-diary-entry :wk "create new diary entry")
    "nt" '(baz/org-journal-new-entry-with-tags :wk "create new entry with tags")
    "nx" '(baz/refile-journal :wk "refile to journal")))

;;;; org-roam 

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory org-notes)
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ;; Dailies
   ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)
  (add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))
  
  (defun baz/org-reuse-windows (&rest _args)
    (when org-roam-buffer-current-node
      (let ((window (get-buffer-window
		     (get-file-buffer
		      (org-roam-node-file org-roam-buffer-current-node)))))
	(when window (select-window window)))))
  (advice-add 'org-roam-node-visit :before #'baz/org-reuse-windows))

;;;; org-download

(use-package org-download
  :config
  (setq-default org-link-file-path-type 'relative)
  (setq-default org-download-image-dir "./data")
  (setq-default org-download-annotate-function (lambda (_) "")))

;;;; org-crypt

(use-package org-crypt
  :ensure nil
  :after org
  :config
  (org-crypt-use-before-save-magic)
  (setq org-crypt-tag-matcher "crypt")
  (setq org-crypt-key "C0FC1B41A828E1FA")
  (setq auto-save-default nil))

;;;; auto-saving
(use-package super-save
  :ensure t
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t)
  (add-to-list 'super-save-predicates (lambda ()
					(if (buffer-file-name)
                                            (string-match-p "org" (buffer-file-name))
                                          nil))))

;;;; org diary publish mode
;; TODO a mode to publish 
;; TODO a function to tangle TODO comments into their own
;; org agenda items

;;; modeline mode
(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-minor-modes t))

;;; CODE
;;;; general 
(use-package outshine)

(use-package prog-mode
  :ensure nil
  :config
  (add-hook 'prog-mode-hook 'outshine-mode)
  (add-hook 'prog-mode-hook 'hs-minor-mode)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

(define-key emacs-lisp-mode-map (kbd "<backtab>") 'outshine-cycle-buffer)

;;;; parens 
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(electric-pair-mode 1)


;;;; flycheck mode
(use-package flycheck
  :diminish 'flycheck-mode
  :config 
  (add-hook 'after-init-hook #'global-flycheck-mode))

;;;; load other files
(load-file (expand-file-name
	    "tab-config.el" user-emacs-directory))

(load-file (expand-file-name
 	    "journal-config.el" user-emacs-directory))

;; modifies all variables in above code so they apply to windows system
;;(load-file (expand-file-name
;;	      "windows-specific.el" user-emacs-directory))
