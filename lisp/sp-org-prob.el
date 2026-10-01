;; -*- lexical-binding: t; -*-
;; org-prob is a collection of functions for my project breakdowns using org mode. Org PROject Breakdown.

(load-file "~/.emacs.d/.org-prob-id.el")

(defun sp/org-prob-new-project ()
  "Create a new project"
  (interactive)
  (let* ((title (read-string "Project title: "))
	 (short-title (read-string "Short title: "))
         (date  (format-time-string "%Y-%m-%d"))
	 (pid (format "%03d" sp/org-prob-project-id))
	 (projectname (concat "P-2026-" pid "-" short-title))
	 (full-title (concat projectname " " title))
	 (dir (concat "~/sync/projects/active/" projectname))
	 (projectile (concat dir "/" ".projectile"))
	 (filename (concat pid "-" short-title ".org"))
	 (filepath (concat "~/sync/projects/prob/" filename)))
    (make-directory dir)
    (find-file filepath)
    (sp/org-prob-insert-template title date full-title)
    (setq sp/org-prob-project-id (1+ sp/org-prob-project-id))
    (write-region
     (format "(setq sp/org-prob-project-id %d)" sp/org-prob-project-id)
     nil
     "~/.emacs.d/.org-prob-id.el")))

(defun sp/org-prob-insert-template (title date project)
  "Insert the org template for a new project"
  (insert "#+TITLE: " title "\n")
  (insert "#+DATE: "  date  "\n")
  (insert "#+AUTHOR: " (user-full-name) "\n")
  (insert "#+STARTUP: overview \n")
  (insert "#+COLUMNS: %25ITEM(Task) %TODO(State) %5Effort(Estimated [min]){:} %Resources(Members) %SCHEDULED %DEADLINE %CLOSED %CLOCKSUM(Clocked) %CLOCKSUM_T(Today)\n")
  (insert "\n* COMMENT Info")
  (insert "\n* COMMENT Reporting\n")
  (insert "#+begin: columnview :hlines 2 :skip-empty-rows \"t\" :indent \"t\" :id \n")
  (insert "#+CAPTION: Overview\n")
  (insert "\n#+end\n")
  (insert "\n#+begin: clocktable :link t :formula %\n")
  (insert "#+end\n")
  (insert "\n* WBS")
  (org-set-property "PROJECT" project)
  (setq wbsid (org-id-get-create))
  (goto-char (+ 3 (search-backward "id")))
  (insert wbsid)
  (goto-char (search-forward ":END:"))
  (insert "\n** TODO ")
  (save-buffer))

(defun sp/org-prob-stuck-projects ()
  "Show active level-2 project headers with no clock activity
(on themselves or direct subheaders) in the last 10 days."
  (interactive)
  (org-ql-search
    (directory-files "~/sync/projects/prob" t "\\.org\\'")
    '(and (todo "PROG")
          (level 2)
          (not (or (clocked :from -10)
                   (children (clocked :from -10)))))
    :title "Stuck projects (no activity in 10 days)"))

(defun sp/--prob-projects (file)
  "Alist (PROJECT . FILE) pour chaque entrée ayant une propriété :PROJECT:."
  (with-temp-buffer
    (insert-file-contents file)
    (delay-mode-hooks (org-mode))
    (delq nil
          (org-map-entries
           (lambda ()
             (when-let ((p (org-entry-get nil "PROJECT")))
               (cons p file)))))))

(defun sp/org-prob-find-project ()
  "Choisit un projet parmi les entrées :PROJECT: et ouvre son fichier."
  (interactive)
  (let* ((files (directory-files "~/sync/projects/prob" t "\\.org\\'"))
         (alist (mapcan #'sp/--prob-projects files))
         (choice (completing-read "Project: " alist nil t)))
    (find-file (cdr (assoc choice alist)))))
    
(global-set-key (kbd "C-c b f") 'sp/org-prob-find-project)    
(global-set-key (kbd "C-c b n") 'sp/org-prob-new-project)
(global-set-key (kbd "C-c b s") 'sp/org-prob-stuck-projects)

(provide 'sp-org-prob)
