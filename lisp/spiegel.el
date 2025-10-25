;; -*- lexical-binding: t; -*-

;;calendar
(defface busy-1 '((t :foreground "white" :background "#607d8b")) "")
(defface busy-2 '((t :foreground "black" :background "white")) "")

(defun highlight-scheduled-days (file face month year indent)
  "Highlight days with scheduled events from the specified file with the given face."
  (dotimes (i 31)
    (let* ((date (list month (1+ i) year))
           (entries (org-agenda-get-day-entries file date)))
      (when entries
        (calendar-mark-visible-date date face)))))

;; hook for highlighting scheduled days in different files with different faces
(defadvice calendar-generate-month
  (after highlight-scheduled-days-advice (month year indent) activate)
  "Highlight days with scheduled events from multiple org files."
  (highlight-scheduled-days "~/orgfiles/agenda.org" 'busy-2 month year indent)
  (highlight-scheduled-days "~/orgfiles/events.org" 'busy-1 month year indent))

;; launch agenda for specific org file (for projects)
;; easier to maintain because the target file can change or not be present (depending on the device I use). If the file was added to org-agenda-files, it would generate an error for the typical org-agenda/dashboard.
(defun specific-proj-agenda()
  "lauch org-agenda for project file"
  (interactive)
  (let ((org-agenda-files '("~/projects/active/PRJ-0001-flat/flat.org" "~/projects/active/PRJ-0002-moving/moving.org"))) (org-agenda))
  )

;; runs gsync shell command
(defun gsync()
  "runs gsync (git autocommit and push) in current directory"
  (interactive)
  (shell-command "sh ~/repos/git-auto/gsync.sh")
  )

(defun sync-notes()
  "git autocommit and push my notes"
  (interactive)
  (shell-command "sh ~/repos/git-auto/push-notes.sh")
  )

(defun sync-notes-and-quit()
  "sync notes and quit Emacs"
  (interactive)
  (save-some-buffers t)
  (sync-notes)
  (message "Notes synced! Quitting in 2 seconds...")
  (sit-for 2) 
  (save-buffers-kill-emacs))

(provide 'spiegel)
