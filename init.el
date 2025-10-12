;; runs script that syncs my notes with git
(start-process "update-notes" "*Messages*" "bash" "-c" "~/repos/git-auto/update-notes.sh")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;basic configuration
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
;;replace yes or no by 'y' or 'n'
(fset 'yes-or-no-p 'y-or-n-p)
(setq visible-bell t)
(electric-pair-mode 1)

(set-frame-font "Iosevka Term 17" nil t)

;; installed package jbeans-theme
;; line 227      `(line-number ((,class (:foreground ,jbeans-grey-5 :background ,jbeans-grey-0)))) before it was 2,  removed grey separation between background and line number

(load-theme 'jbeans t)
;(load-theme 'modus-operandi-tinted t)

;send auto-save files to another directory
(setq backup-directory-alist '(("." . "~/backup")))
(with-eval-after-load 'tramp
      (add-to-list 'tramp-backup-directory-alist
		   (cons tramp-file-name-regexp nil)))

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
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

;;appearance
(use-package dashboard
  :ensure t
  :config
    (dashboard-setup-startup-hook)
    (setq dashboard-startup-banner "~/.emacs.d/img/onepiece.png")
    (setq dashboard-items '((agenda . 4)
                        (recents  . 4)))
    (setq dashboard-banner-logo-title "Parfois, le cœur se serre à la pensée des choses qui auraient pu être et qui n'ont pas été"))

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (setq powerline-default-separator (quote arrow))
  (spaceline-spacemacs-theme))

;;ui improvements
(use-package swiper
  :ensure t)
  
(use-package ivy
  :ensure t
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (setq ivy-use-selectable-prompt t)
  (ivy-mode 1))

(use-package counsel
  :bind (("C-x b" . 'counsel-switch-buffer)
         ("C-x f" . 'counsel-find-file)
	 ("M-x" . 'counsel-M-x)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(use-package ivy-rich
  :ensure t
  :init
  (ivy-rich-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; install fonts, run all-the-icons-install-fonts just after, see doc
(use-package all-the-icons
  :if (display-graphic-p)
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :hook ((dired-mode . all-the-icons-dired-mode)
         (dired-mode . dired-hide-details-mode)))

;; see doc, install cmake and libtool-bin
(use-package vterm
  :ensure t
  :init
  (global-set-key (kbd "C-c <return>") 'vterm))

(add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode 0)))

;; project management
;; also installed ripgrep on terminal to use counsel-projectile-rg
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/repos")
    (setq projectile-project-search-path '("~/repos")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;;org
(use-package org
  :config
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-adapt-indentation t)
  (setq org-agenda-files
	'("~/orgfiles/todo.org"
	  "~/orgfiles/events.org"
	  "~/orgfiles/agenda.org"))
  (setq org-agenda-inhibit-startup t)
  (require 'org-habit)
   (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)
  (setq org-habit-show-habits-only-for-today nil)

  (setq org-todo-keywords
	'((sequence "TODO(t)" "ONHOLD(h)" "DOCUMENT(w)" "DOING(a)" "|" "DONE(d)" "CANC(c)")))
  (setq org-todo-keyword-faces
        '(("DOING" . "orange") ("ONHOLD" . "grey") ("DOCUMENT" . "red")))
  (setq org-refile-targets
	'(("~/backup/archive.org" :maxlevel . 1)))
  ;; Save Org buffers after refiling
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-agenda-custom-commands 
   '(("v" "Event" tags "event")
     ("w" "Workflow Status"
     ((todo "TODO"
            ((org-agenda-overriding-header "BACKLOG")
             (org-agenda-files org-agenda-files)))
      (todo "ONHOLD"
            ((org-agenda-overriding-header "ON HOLD")
             (org-agenda-files org-agenda-files)))
      (todo "DOING"
            ((org-agenda-overriding-header "IN PROGRESS")
             (org-agenda-files org-agenda-files)))
      (todo "DOCUMENT"
            ((org-agenda-overriding-header "WRITE DOCUMENTATION")
             (org-agenda-files org-agenda-files)))
      (todo "DONE"
            ((org-agenda-overriding-header "FINISHED")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "CANCELLED")
             (org-agenda-files org-agenda-files)))))))
  ;; Define Org Capture templates
  (setq org-capture-templates
      '(("t" "task")
        ("tt" "task" entry (file "~/orgfiles/todo.org")
         "* TODO %?")
        ("tl" "task with link" entry (file "~/orgfiles/todo.org")
         "* TODO %?\n  %a\n  %i")
	
	("l" "ledger entry")	
	("lc" "paiement CE VISA" plain
                (file "~/finance/2025/journal.dat")
	        "%(org-read-date) * %^{Payee} 
  expenses:%^{Account}  %^{Amount} EUR
  liabilities:CEbank:visa")
	("ld" "debit differe" plain
                (file "~/finance/2025/journal.dat")
	        "%(org-read-date) * debit differe 
  liabilities:CEbank:visa  %^{Amount} EUR
  assets:CEbank:compte")

	("n" "quick note")
	("nn" "note" entry (file "~/orgfiles/refile.org")
	 "* %?")
        ("nl" "note with link" entry (file "~/orgfiles/refile.org")
         "* %?\n  %a\n  %i")

	("e" "event")
	("ee" "add event" entry (file "~/orgfiles/events.org")
	 "* %? :event:")
	
	("a" "agenda")
	("aa" "add item in agenda" entry (file "~/orgfiles/agenda.org")
	 "* %?"))))

(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda ()
                             (org-bullets-mode 1)))) 

;use aplay or install paplay
(use-package org-pomodoro
   :commands org-pomodoro
   :config
   (setq org-pomodoro-audio-player "/usr/bin/paplay")
   (setq org-pomodoro-short-break-sound "~/.emacs.d/sounds/three_beeps.wav")
   (setq org-pomodoro-long-break-sound "~/.emacs.d/sounds/three_beeps.wav")
   (setq org-pomodoro-finished-sound "~/.emacs.d/sounds/zelda.wav"))

;<s TAB to generate quickly a code block
(require 'org-tempo)
;export org files to markdown
(require 'ox-md)

;;org roam
;;might need to install a c compiler [see doc]
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/notes"))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
    '(("d" "default" plain
      "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
      ("m" "meeting" plain
      (file "~/.emacs.d/templates/meeting_note_template.org")
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :clock-in :clock-resume
      :unnarrowed t)
      ("r" "recipe" plain
      (file "~/.emacs.d/templates/recipe_template.org")
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
      ("b" "book" plain
      (file "~/.emacs.d/templates/book_note_template.org")
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
;   ("C-c n c" . org-roam-capture)
   ("C-c n l" . org-roam-buffer-toggle)
         ;; Dailies
   ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :ensure t
  :after org-roam)

;;org-babel to execute codes in org buffers
;install ditaa to use it in org mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (emacs-lisp . t)
   (ditaa . t)))

;;tells org babel where to find the ditaa.jar file
(setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")
;use visual-line-mode for org files
(add-hook 'org-mode-hook #'visual-line-mode)

;;inhibit electric pair mode for <>
(add-hook 'org-mode-hook (lambda ()
           (setq-local electric-pair-inhibit-predicate
                   `(lambda (c)
                  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

(use-package gnuplot
  :ensure t)

;; calendar
;; Define faces for different file colors
(defface busy-1 '((t :foreground "white" :background "#607d8b")) "")
(defface busy-2 '((t :foreground "black" :background "white")) "")

(defun highlight-scheduled-days (file face month year indent)
  "Highlight days with scheduled events from the specified file with the given face."
  (dotimes (i 31)
    (let* ((date (list month (1+ i) year))
           (entries (org-agenda-get-day-entries file date)))
      (when entries
        (calendar-mark-visible-date date face)))))

;; Hook for highlighting scheduled days in different files with different faces
(defadvice calendar-generate-month
  (after highlight-scheduled-days-advice (month year indent) activate)
  "Highlight days with scheduled events from multiple org files."
  (highlight-scheduled-days "~/orgfiles/agenda.org" 'busy-2 month year indent)
  (highlight-scheduled-days "~/orgfiles/events.org" 'busy-1 month year indent))

; launch agenda for specific org file (for projects)
; easier to maintain because the target file can change or not be present (depending on the device I use). If the file was added to org-agenda-files, it would generate an error for the typical org-agenda/dashboard.
(defun specific-proj-agenda()
  "lauch org-agenda for project file"
  (interactive)
  (let ((org-agenda-files '("~/projects/active/PRJ-0001-flat/flat.org" "~/projects/active/PRJ-0002-moving/moving.org"))) (org-agenda))
  )

(defun gsync()
  "runs gsync (git autocommit and push) in current directory"
  (interactive)
  (shell-command "sh ~/repos/git-auto/gsync.sh")
  )

;; finance
(use-package ledger-mode
  :ensure t
  :init
  (setq ledger-clear-whole-transactions 1)
  :mode "\\.dat\\'")

;;keybindings
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'calendar)
(global-set-key "\C-cv" 'visual-line-mode)
(global-set-key "\C-cd" 'copy-from-above-command)
(global-set-key "\C-cf" 'org-pomodoro)
(global-set-key "\C-cs" 'specific-proj-agenda)
(global-set-key "\C-cg" 'gsync)
