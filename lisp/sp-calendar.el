;; -*- lexical-binding: t; -*-
;;;; calendar

(defface calendar-event
  '((t :foreground "white" :background "#607d8b"))
  "Face for days with :event: tagged org entries.")

(defface calendar-meeting
  '((t :foreground "black" :background "#b0bec5"))
  "Face for days with :meeting: tagged org entries.")

(defface calendar-trip
  '((t :foreground "black" :background "white"))
   "Face for days with :trip: tagged org entries.")

(defun sp/calendar-highlight-by-tags (file month year)
  "Highlight days in calendar based on org tags from FILE for MONTH/YEAR.
:meeting: tag uses `calendar-meeting' face, :event: tag uses `calendar-event' face."
  (dotimes (i 31)
    (let* ((day (1+ i))
           (date (list month day year)))
      (when (calendar-date-is-valid-p date)
        (dolist (entry (org-agenda-get-day-entries file date))
          (let ((tags (get-text-property 0 'tags entry)))
            (cond
             ((member "meeting" tags)
              (calendar-mark-visible-date date 'calendar-meeting))
	     ((member "trip" tags)
              (calendar-mark-visible-date date 'calendar-trip))
             ((member "event" tags)
              (calendar-mark-visible-date date 'calendar-event)))))))))

(defun sp/calendar-visualize-agenda ()
  "Open calendar and highlight days by org tag from agenda.org.
Only highlights when called explicitly — does not affect the default calendar."
  (interactive)
  (require 'org)          ; runs the :config of your use-package org block
  (require 'org-agenda)
  (let ((agenda-file (expand-file-name "~/sync/orgfiles/agenda.org")))
    (unless (file-readable-p agenda-file)
      (user-error "Agenda file not found: %s" agenda-file))
    (calendar)
    (let* ((today (calendar-current-date))
           (month (calendar-extract-month today))
           (year  (calendar-extract-year  today)))
      (dolist (offset '(-1 0 1))   ; highlight prev, current, next month
        (let ((m (+ month offset))
              (y year))
          (cond ((< m 1)  (setq m (+ m 12) y (1- y)))
                ((> m 12) (setq m (- m 12) y (1+ y))))
          (sp/calendar-highlight-by-tags agenda-file m y))))))

;;;; past agenda entries

(defvar sp/agenda-file (expand-file-name "~/sync/orgfiles/agenda.org")
  "Org file holding calendar entries.")

(defun sp/agenda--entry-last-date ()
  "Return the latest active timestamp date (\"YYYY-MM-DD\") in the entry at point.
Looks at the whole subtree, so ranges <a>--<b> count by their end date.
Returns nil when the entry has no active timestamp."
  (let ((end (save-excursion (org-end-of-subtree t t)))
        last)
    (save-excursion
      (while (re-search-forward "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)[^>\n]*>" end t)
        (let ((d (match-string-no-properties 1)))
          (when (or (null last) (string> d last))
            (setq last d)))))
    last))

(defun sp/agenda-past-entries (&optional erase file)
  "Return top-level entries of FILE (default `sp/agenda-file') that are in the past.
An entry is past when its latest active timestamp is before today; entries
without a timestamp are ignored.  Each element is (HEADING DATE TEXT).
With ERASE non-nil (interactively: prefix arg), also delete those entries
from FILE and save it, after confirmation."
  (interactive "P")
  (require 'org)
  (let* ((file (or file sp/agenda-file))
         (today (format-time-string "%Y-%m-%d"))
         found)
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward "^\\* " nil t)
         (beginning-of-line)
         (let ((beg (point))
               (date (sp/agenda--entry-last-date))
               (end (save-excursion (org-end-of-subtree t t))))
           (when (and date (string< date today))
             (push (list (org-get-heading t t t t) date
                         (buffer-substring-no-properties beg end)
                         beg end)
                   found))
           (goto-char end))))
      ;; FOUND is in reverse buffer order, which is what we want for deleting.
      (when (and erase found
                 (yes-or-no-p (format "Erase %d past entries from %s? "
                                      (length found)
                                      (file-name-nondirectory file))))
        (dolist (e found)
          (delete-region (nth 3 e) (nth 4 e)))
        (save-buffer)))
    (setq found (mapcar (lambda (e) (list (nth 0 e) (nth 1 e) (nth 2 e)))
                        (nreverse found)))
    (when (called-interactively-p 'any)
      (with-current-buffer (get-buffer-create "*Past agenda entries*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dolist (e found) (insert (nth 2 e)))
          (org-mode)
          (view-mode 1)           ; read-only, `q' quits the window
          (goto-char (point-min)))
        (display-buffer (current-buffer)))
      (message "%d past entries%s" (length found) (if erase " (erase requested)" "")))
    found))

(provide 'sp-calendar)
