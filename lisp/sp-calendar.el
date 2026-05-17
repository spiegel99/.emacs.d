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

(provide 'sp-calendar)
