;; following Systems Crafters config
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))
;; Profile emacs startup
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))



;; Initialize package sources
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
(defvar baz/default-font-size 220)
(set-face-attribute 'default nil :font "JetBrains Mono" :height baz/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height baz/default-font-size)
(set-face-attribute 'variable-pitch nil :font "JetBrains Mono" :height baz/default-font-size :weight 'regular)

(load-theme 'modus-vivendi)
(define-key global-map (kbd "<f5>") #'modus-themes-toggle)

(use-package olivetti
  :demand t
  :init
  (setq olivetti-body-width 80)
  (setq olivetti-style 'fancy)
  (setq olivetti-minimum-body-width 50))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; VARIABLES
(defvar baz/org-path "c:/Users/HughesDavA/Documents/org/")

;; MISC 
(use-package emacs
  :demand t
  :ensure nil
  :init
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq inhibit-startup-message t)
  (scroll-bar-mode -1)        
  (tool-bar-mode -1)          
  (tooltip-mode -1)          
  (menu-bar-mode -1)        
  (setq visible-bell nil)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
)


;; KEYBINDING MANAGERS
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  ;;(setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection ;; evilifies a bunch of things
  :after evil
  :init
  (setq evil-collection-outline-bind-tab-p t) ;; '<TAB>' cycles visibility in 'outline-minor-mode'
  ;; If I want to incrementally enable evil-collection mode-by-mode, I can do something like the following:
  ;; (setq evil-collection-mode-list nil) ;; I don't like surprises
  ;; (add-to-list 'evil-collection-mode-list 'magit) ;; evilify magit
  ;; (add-to-list 'evil-collection-mode-list '(pdf pdf-view)) ;; evilify pdf-view
  :config
  (evil-collection-init))

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
    "w" '(:keymap evil-window-map :wk "window")) ;; window bindings

  (baz/leader-keys
    "s" '(:ignore t :wk "search"))

  (baz/leader-keys
    "c" '(:ignore t :wk "code"))

  ;; help
  ;; namespace mostly used by 'helpful'
  (baz/leader-keys
    "h" '(:ignore t :wk "help"))

  ;; file
  (baz/leader-keys
    "f" '(:ignore t :wk "file")
    "ff" '(find-file :wk "find file") ;; gets overridden by consult
    "fs" '(save-buffer :wk "save file"))

  ;; buffer
  ;; see 'bufler' and 'popper'
  (baz/leader-keys
    "b" '(:ignore t :wk "buffer")
    "bb" '(switch-to-buffer :wk "switch buffer") ;; gets overridden by consult
    "bk" '(kill-this-buffer :wk "kill this buffer")
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

  ;; buffer list
  (baz/leader-keys
    "," '(switch-to-buffer :wk "switch buffer"))

  ;; open
  (baz/leader-keys
    "o" '(:ignore t :wk "open")
    "os" '(speedbar t :wk "speedbar")) ;; TODO this needs some love

  ;; toggle
  (baz/leader-keys
    "t" '(:ignore t :wk "toggle")
    "tt" '(tab-bar-mode :wk "toggle tab bar mode")
    "to" '(olivetti-mode :wk "toggle olivetti mode")) 

  ;; search
  ;; see 'consult'
  (baz/leader-keys
    "s" '(:ignore t :wk "search"))

  ;; open
  (baz/leader-keys
    "o-" '(dired-jump :wk "open in dired")) 

  ;; templating
  ;; see 'tempel'
  (baz/leader-keys
    "t" '(:ignore t :wk "template")))


;;(setq use-package-verbose t)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; configuration taken from prelude configuration
(keymap-global-set "C-c j" 'org-journal-new-entry)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)



;; COMPLETION FRAMEWORK
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


;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package consult
  :demand t
  :ensure t
  :general
  (baz/leader-keys
    "bb" '(consult-buffer :wk "consult buffer")
    "Bb" '(consult-bookmark :wk "consult bookmark")
    ;; "ht" '(consult-theme :wk "consult theme")
    "sr" '(consult-ripgrep :wk "consult rg")
    "sg" '(consult-grep :wk "consult grep")
    "sG" '(consult-git-grep :wk "consult git grep")
    "sf" '(consult-find :wk "consult find")
    "sF" '(consult-locate :wk "consult locate")
    "sl" '(consult-line :wk "consult line")
    "sy" '(consult-yank-from-kill-ring :wk "consult yank from kill ring")
    "i" '(consult-imenu :wk "consult imenu"))
  :config
  ;; use project.el to retrieve the project root
  ;; (set consult-project-root-function
  ;;       (lambda ()
  ;;         (when-let (project (project-current))
  ;;           (car (project-roots project)))))
  )


;; NAVIGATION
(use-package perspective
  :ensure t  ; use `:straight t` if using straight.el!
  :bind
  (("C-x C-b" . persp-list-buffers))
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))
  :init
  (persp-mode))


;; ESSENTIAL TOOLS 
(use-package org
  :ensure t
  :demand t
  :init

  ;; todo setup
  (setq org-todo-keywords
        '((sequence "TODO" "NEXT" "SOMEDAY" "|" "DONE(d)" "CANCELLED")
	  (sequence "PROJ" "|" "COMPLETED")))

  (setq org-adapt-indentation nil)   ;; interacts poorly with 'evil-open-below'

  :general
  (baz/local-leader-keys
        :keymaps 'org-mode-map
        "a" '(org-archive-subtree :wk "archive")
        "l" '(:ignore t :wk "link")
        "ll" '(org-insert-link t :wk "link")
        "lp" '(org-latex-preview t :wk "prev latex")
        "h" '(consult-org-heading :wk "consult heading")
        "d" '(org-cut-special :wk "org cut special")
        "y" '(org-copy-special :wk "org copy special")
        "p" '(org-paste-special :wk "org paste special")
        "b" '(:keymap org-babel-map :wk "babel")
        "t" '(org-todo :wk "todo")
        "s" '(org-insert-structure-template :wk "template")
        "e" '(org-edit-special :wk "edit")
        "i" '(:ignore t :wk "insert")
        "ih" '(org-insert-heading :wk "insert heading")
        "is" '(org-insert-subheading :wk "insert heading")
        "f" '(org-footnote-action :wk "footnote action")
        ">" '(org-demote-subtree :wk "demote subtree")
        "<" '(org-promote-subtree :wk "demote subtree"))

  :hook
  (org-mode . olivetti-mode)
  (org-mode . variable-pitch-mode)

  :config
  ;; set up org paths
  (setq org-directory "c:/Users/HughesDavA/Documents/org/")
  (setq org-default-notes-file (concat org-directory "/todo.org")))

(use-package org-journal
  :ensure t
  :defer t
  :config
  (setq org-journal-dir (concat baz/org-path)
	org-journal-file-type 'yearly
	org-journal-file-format "%Y-%m-%d.org"
	org-journal-enable-agenda-integration t
	org-extend-today-until 4
	org-journal-date-format "%a, %Y-%m-%d")
  :general
  (baz/leader-keys
  "nj" '(org-journal-new-entry :wk "create new entry")))

(use-package magit
  :ensure t
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-git-executable "c:/Program Files/Git/bin/git")
  :general
  (baz/leader-keys
    "g" '(:ignore t :wk "git")
    "gg" '(magit-status :wk "magit status")))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("c:/home/alex/org/journal/2024-06-03.org" "c:/Users/HughesDavA/Documents/org/2024-01-01.org"))
 '(package-selected-packages
   '(magit consult general which-key vertico undo-tree perspective org-journal olivetti marginalia hydra evil-collection doom-modeline)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
