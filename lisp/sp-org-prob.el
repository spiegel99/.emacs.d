;; -*- lexical-binding: t; -*-
;; org-prob is a collection of functions for my project breakdowns using org mode. Org PROject Breakdown.

(load-file "~/.emacs.d/.org-prob-id.el")

(defun sp/org-prob-files ()
  "Return the project files in `sp/org-prob-directory'."
  (directory-files sp/org-prob-directory t "\\.org\\'"))

(defun sp/org-prob-new-project ()
  "Create a new project"
  (interactive)
  (let* ((title (read-string "Project title: "))
	 (short-title (read-string "Short title: "))
         (date  (format-time-string "%Y-%m-%d"))
	 (pid (format "%03d" sp/org-prob-project-id))
	 (projectname (concat "P-" (format-time-string "%y") "-" pid "-" short-title))
	 (full-title (concat projectname " " title))
	 (dir (concat "~/sync/projects/" projectname))
	 (filename (concat pid "-" short-title ".org"))
	 (filepath (expand-file-name filename sp/org-prob-directory)))
    (make-directory dir)
    (find-file filepath)
    (sp/org-prob-insert-template full-title date)
    (setq sp/org-prob-project-id (1+ sp/org-prob-project-id))
    (write-region
      (format ";; -*- lexical-binding: t; -*-\n(setq sp/org-prob-project-id %d)\n"
         sp/org-prob-project-id)
     nil
     "~/.emacs.d/.org-prob-id.el")))

(defun sp/org-prob-insert-template (title date)
  "Insert the org template for a new project"
  (insert "#+title: " title "\n")
  (insert "#+date: "  date  "\n")
  (insert "#+author: " (user-full-name) "\n")
  (insert "#+startup: overview \n")
  (insert "#+columns: %25ITEM(Task) %TODO(State) %5Effort(Estimated [min]){:} %Resources(Members) %SCHEDULED %DEADLINE %CLOSED %CLOCKSUM(Clocked) %CLOCKSUM_T(Today)\n")
  (insert "\n* COMMENT Reporting\n")
  (insert "#+begin: columnview :hlines 2 :skip-empty-rows \"t\" :indent \"t\" :id \n")
  (insert "#+caption: Overview\n")
  (insert "\n#+end\n")
  (insert "\n#+begin: clocktable :link t :formula %\n")
  (insert "#+end\n")
  (insert "\n* WBS")
  (org-set-property "ORDERED" "t")
  (let ((wbsid (org-id-get-create)))
    (goto-char (+ 3 (search-backward "id")))
    (insert wbsid))
  (goto-char (search-forward ":END:"))
  (insert "\n** TODO ")
  (save-buffer))

(defun sp/org-prob-stuck-projects ()
  "Show active tasks in my projects with no clock activity in the last 10 days."
  (interactive)
  (org-ql-search
    (sp/org-prob-files)
    '(and (todo "PROG")
	  (not (clocked :from -10)))
    :title "Stuck projects (no activity in 10 days)"))

(defun sp/--prob-title (file)
  "Return the #+TITLE of FILE, or its base name if it has none.
Only the first 1 KB is read, the title is always at the top of the file."
  (with-temp-buffer
    (insert-file-contents file nil 0 1024)
    (let ((case-fold-search t))
      (if (re-search-forward "^#\\+title:[ \t]*\\(.+\\)$" nil t)
          (string-trim (match-string 1))
        (file-name-base file)))))

(defun sp/org-prob-find-project ()
  "Choose a project by its #+TITLE and open its file."
  (interactive)
  (let* ((alist (mapcar (lambda (f) (cons (sp/--prob-title f) f))
                        (sp/org-prob-files)))
         (choice (completing-read "Project: " alist nil t)))
    (find-file (cdr (assoc choice alist)))))
    
(global-set-key (kbd "C-c b f") 'sp/org-prob-find-project)    
(global-set-key (kbd "C-c b n") 'sp/org-prob-new-project)
(global-set-key (kbd "C-c b s") 'sp/org-prob-stuck-projects)

(provide 'sp-org-prob)
