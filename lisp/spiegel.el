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
  (highlight-scheduled-days "~/sync/orgfiles/agenda.org" 'busy-2 month year indent)
  (highlight-scheduled-days "~/sync/orgfiles/events.org" 'busy-1 month year indent))


;; productivity workflow

(defun current-week ()
  "Insert current ISO week for meeting notes index."
  (interactive)
  (insert (format-time-string "* week %V of %Y - %d/%m")))

(defun stuck-projects ()
  "Show active tasks with no clock activity in the last 10 days."
  (interactive)
  (org-ql-search
    (org-agenda-files)
    '(and (todo "PROG")
          (not (clocked :from -10)))
    :title "Stuck projects (no activity in 10 days)"))

(defun my-projects ()
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

(provide 'spiegel)
