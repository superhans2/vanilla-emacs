;; copying tab bar navigation code 
(defhydra mmk2410/tab-bar (:color teal) 
  "My tab-bar helpers"
  ("j" mmk2410/tab-bar-run-journal "Org")
  ("i" baz/tab-bar-run-config "Config")
  ("c" nil "cancel"))

(global-set-key (kbd "C-c f") 'mmk2410/tab-bar/body)

(defun mmk2410/tab-bar-switch-or-create (name func)
  (if (mmk2410/tab-bar-tab-exists name)
      (tab-bar-switch-to-tab name)
    (mmk2410/tab-bar-new-tab name func)))

(defun mmk2410/tab-bar-tab-exists (name)
  (member name
          (mapcar #'(lambda (tab) (alist-get 'name tab))
                  (tab-bar-tabs))))

(defun mmk2410/tab-bar-new-tab (name func)
  (when (eq nil tab-bar-mode)
    (tab-bar-mode))
  (tab-bar-new-tab)
  (tab-bar-rename-tab name)
  (funcall func))

(defun mmk2410/tab-bar-run-journal ()
  (interactive)
  (mmk2410/tab-bar-switch-or-create
   "Org"
   #'org-journal-open-current-journal-file))

(defun baz/tab-bar-run-config ()
  (interactive)
  (mmk2410/tab-bar-switch-or-create
   "Config"
   (lambda () (find-file "/home/alex/vanilla-emacs/init.el"))))

(defun baz/startup ()
  (progn
    ;; setting up Org tab
    (tab-rename "Org")
    (org-journal-open-current-journal-file)
    (evil-window-vsplit 60)
    (other-window 1)
    (find-file (concat org-directory "/inbox.org"))))

;; emacs startup hook
(add-hook 'window-setup-hook 'baz/startup)
