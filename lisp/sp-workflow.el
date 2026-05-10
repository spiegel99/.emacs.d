;; -*- lexical-binding: t; -*-

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
