;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


;;; INITIAL

(setq display-line-numbers-type t)
(setq org-directory "~/org/")


(setq user-full-name "Alex Hughes"
      user-mail-address "ashughes001@gmail.com"
      major-mode 'emacs-lisp-mode
      doom-theme 'modus-operandi
      display-line-numbers-type nil
      load-prefer-newer t)

;;;; APPEARANCE

(setq doom-font (font-spec :family "JetBrains Mono" :size 24)
    doom-big-font (font-spec :family "JetBrains Mono" :size 36)
    doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 24)
    doom-variable-pitch-font (font-spec :family "Roboto" :size 20)
    doom-unicode-font (font-spec :family "JuliaMono")
    doom-serif-font (font-spec :family "IBM Plex Mono" :weight 'light))


(global-set-key (kbd "<escape>") 'doom/escape)

(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(setq projectile-auto-discover 't
      projectile-project-search-path '("~/code/" "~/Documents/" ))

(setq +workspaces-switch-project-function
      (lambda (default-directory)
        (magit-status)))

;;;; ORG

(load-file (expand-file-name
 	    "shared.el" "~/vanilla-emacs/"))

(setq org-roam-directory org-notes)

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))


(map! :leader
      "x" #'org-capture
      "X" #'doom/open-scratch-buffer)

;;;;; bibliography management

(setq! citar-bibliography (list zot-bib)
        citar-notes-paths (list (concat org-notes "/references/"))
        org-cite-global-bibliography citar-bibliography
        ;; citar-at-point-function 'embark-act
        citar-file-open-function 'citar-file-open-external)


;;;;; journal

(after! org-journal
  (setq
      org-journal-dir (concat org-directory "/journal")
      org-journal-file-type 'monthly
      org-journal-file-format "%Y-%m.org"
      org-journal-time-format "%R "
      org-journal-carryover-items ""
      org-journal-enable-agenda-integration t
      org-extend-today-until 4
      org-journal-date-format "%a, %Y-%m-%d"))

(global-set-key (kbd "C-c f") 'org-roam-node-find)
(global-set-key (kbd "C-c i") 'org-roam-node-insert)
(global-set-key (kbd "C-c I") 'org-roam-node-insert-immediate)

(map! :leader
      ";" #'counsel-M-x
      ":" #'pp-eval-expression
      :prefix "r"
      "i" #'org-roam-node-insert
      "f" #'org-roam-node-find
      "r" #'org-roam-buffer-toggle
      "t" #'org-roam-tag-add
      :prefix "t"
      "t" #'tab-bar-mode
      "o" #'olivetti-mode
      :prefix "n"
      "j" #'org-journal-new-entry)

(defadvice! +org-roam-reuse-windows (&rest r)
  :before #'org-roam-preview-visit
  :before #'org-roam-node-visit
  (when org-roam-buffer-current-node
    (let ((window (get-buffer-window
                    (get-file-buffer
                      (org-roam-node-file org-roam-buffer-current-node)))))
      (when window (select-window window)))))

;; (use-package! websocket :after org-roam)
;; (use-package! org-roam-ui
;;     :after org-roam
;;     :config
;;     (setq org-roam-ui-sync-theme t
;;           org-roam-ui-follow t
;;           org-roam-ui-update-on-save nil  ;; this causing serious slowdown upon saving
;;           org-roam-ui-open-on-start t))

(setq org-roam-capture-templates
  '(("d" "default" plain "%?"
     :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+title: ${title}\n")
     :unnarrowed t)))

(after! org-roam
  (setq org-roam-list-files-commands '(find fd fdfind rg)))

(defadvice! yeet/org-roam-in-own-workspace-a (&rest _)
  "Open all roam buffers in there own workspace."
  :before #'org-roam-node-find
  :before #'org-roam-node-random
  :before #'org-roam-buffer-display-dedicated
  :before #'org-roam-buffer-toggle
  (when (featurep! :ui workspaces)
    (+workspace-switch "ZK" t)))

;;;; displaying workspaces
(after! persp-mode
  ;; alternative, non-fancy version which only centers the output of +workspace--tabline
  (defun workspaces-formatted ()
    (+doom-dashboard--center (frame-width) (+workspace--tabline)))

  (defun hy/invisible-current-workspace ()
    "The tab bar doesn't update when only faces change (i.e. the
current workspace), so we invisibly print the current workspace
name as well to trigger updates"
    (propertize (safe-persp-name (get-current-persp)) 'invisible t))

  (customize-set-variable 'tab-bar-format '(workspaces-formatted tab-bar-format-align-right hy/invisible-current-workspace))

  ;; don't show current workspaces when we switch, since we always see them
  ;; (advice-add #'+workspace/display :override #'ignore)
  ;; same for renaming and deleting (and saving, but oh well)
  ;; (advice-add #'+workspace-message :override #'ignore))
  )

;; need to run this later for it to not break frame size for some reason
(run-at-time nil nil (cmd! (tab-bar-mode +1)))
