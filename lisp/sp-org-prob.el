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
	 (filepath (concat dir "/" filename)))
    (make-directory dir)
    (find-file projectile)
    (save-buffer)
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
  (insert "#+COLUMNS: %25ITEM(Task) %TODO(State) %5Effort(Estimated [min]){:} %Resources(Members) %SCHEDULED %DEADLINE %CLOSED %CLOCKSUM(Clocked) %CLOCKSUM_T(Today)\n")
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
  "Show active tasks in my projects with no clock activity in the last 10 days."
  (interactive)
  (org-ql-search
    (org-agenda-files)
    '(and (todo "PROG")
          (not (clocked :from -10)))
    :title "Stuck projects (no activity in 10 days)"))

(defun sp/org-prob-find-project ()
  "Find project"
  (interactive)
  (let* ((proj
          (seq-uniq
           (org-ql-select (org-agenda-files)
             '(property "PROJECT")
             :action '(org-entry-get (point) "PROJECT"))))
         (cat (completing-read "Project: " proj)))
    (org-ql-search (org-agenda-files)
      `(and (todo "TODO" "PROG" "HOLD" "DOC")
            (property "PROJECT" ,cat :inherit t))
      :title (format "Project: %s" cat))))

(provide 'sp-org-prob)
