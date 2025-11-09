;;;;  EMACS VERSION 30.1 AT LEAST RECOMMENDED
;; runs script that syncs my notes with git
(start-process "update-notes" "*Messages*" "bash" "-c" "~/repos/git-auto/pull-notes.sh")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-to-list 'load-path '"~/.emacs.d/lisp")
(require 'spiegel)
 
;;;; +---------------------+
;;;; | basic configuration |
;;;; +---------------------+

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
(setq electric-pair-inhibit-predicate
      (lambda (c)
        (or (char-equal c ?<)
            (electric-pair-default-inhibit c))))

(set-frame-font "Iosevka Term 17" nil t)

;; installed package ujelly-theme
(load-theme 'ujelly t)

;;send auto-save files to another directory
(setq backup-directory-alist '(("." . "~/backup")))

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;;; +--------------------------------+
;;;; | appearance and ui improvements |
;;;; +--------------------------------+

(use-package mood-line
  :config
  (mood-line-mode))

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

;; install fonts: run all-the-icons-install-fonts just after package is installed, see doc
(use-package all-the-icons
  :if (display-graphic-p)
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :hook ((dired-mode . all-the-icons-dired-mode)
         (dired-mode . dired-hide-details-mode)))

;;;; +-------------+
;;;; | programming |
;;;; +-------------+

;; see doc, install cmake and libtool-bin
(use-package vterm
  :ensure t
  :init
  (global-set-key (kbd "C-c <return>") 'vterm))

(add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode 0)))

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

(use-package treemacs
  :ensure t
  :config
  (setq treemacs-width 25)
  :bind
  (("C-c t" . treemacs))
   :hook (treemacs-mode . (lambda ()
                           (display-line-numbers-mode 0))))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

;; emacs built-in auto-completion
(global-completion-preview-mode 1)

(use-package eglot
  :bind (("C-c e c" . eglot-reconnect)
         ("C-c e l" . eglot)
         ("C-c e r n" . eglot-rename)
         ("C-c e s" . eglot-shutdown)))

(add-hook 'python-mode-hook 'eglot-ensure) 

(use-package poetry
  :ensure t)
;  :hook (python-mode . poetry-tracking-mode))

;;inhibit eldoc
(setq eldoc-echo-area-use-multiline-p nil)

;;;; +-----+
;;;; | org |
;;;; +-----+

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
   (python . t)
   (ditaa . t)))

(setq org-babel-python-command "python3")
;;tells org babel where to find the ditaa.jar file
(setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")
;use visual-line-mode for org files
(add-hook 'org-mode-hook #'visual-line-mode)

;; to visualize my site
(use-package simple-httpd
  :ensure t)

;;;; +---------+
;;;; | finance |
;;;; +---------+

(use-package ledger-mode
  :ensure t
  :init
  (setq ledger-clear-whole-transactions 1)
  :mode "\\.dat\\'")

;;;; keybindings

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'calendar)
(global-set-key "\C-cv" 'visual-line-mode)
(global-set-key "\C-cd" 'copy-from-above-command)
(global-set-key "\C-cf" 'org-pomodoro)
(global-set-key "\C-cs" 'specific-proj-agenda)
(global-set-key "\C-cg" 'gsync)
(global-set-key "\C-cx" 'sync-notes-and-quit)
(global-set-key "\C-cy" 'poetry)
