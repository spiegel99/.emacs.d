;; -*- lexical-binding: t; -*-

;;;; calendar
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


;;;; productivity workflow

(defun sp/open-orgfiles ()
  "Open orgfiles directory"
  (interactive)
  (dired "~/sync/orgfiles")
  (message "opened orgfiles directory"))

(defun sp/open-ledger-dir ()
  "Open ledger directory"
  (interactive)
  (dired "~/sync/ledger")
  (message "opened ledger directory"))

; to be used to align quickly the table in meeting notes.
(defun sp/org-table-align-backward ()
  "Align closest table backward. Function written for my meeting notes"
  (save-excursion
  (let ((back (search-backward "|")))
    (when back
      (goto-char back)
      (org-table-align)
      (message "aligned table position %d" back)))))

;; Line number: Automatic toggling between line number modes
(defun sp/update-line-numbers-all-windows ()
  "Relative numbers in selected window, absolute in all others."
  (dolist (win (window-list))
    (with-current-buffer (window-buffer win)
      (when display-line-numbers-mode
        (setq-local display-line-numbers
                    (if (eq win (selected-window)) 'relative t))))))

;; BufEnter / WinEnter equivalent
(add-hook 'window-selection-change-functions
          (lambda (_) (sp/update-line-numbers-all-windows)))

;; FocusGained / FocusLost equivalent
(add-hook 'focus-in-hook  #'sp/update-line-numbers-all-windows)
(add-hook 'focus-out-hook #'sp/update-line-numbers-all-windows)
(global-display-line-numbers-mode 1)

(provide 'sp-workflow)
