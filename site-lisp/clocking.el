(require 'magit)

(defun clocking/get-week-year (&optional ask-for-date)
  (let* ((date (calendar-current-date))
         (iso-date (if ask-for-date
                       (car (calendar-iso-read-date))
                     (calendar-iso-from-absolute
                      (calendar-absolute-from-gregorian date))))
         (week (nth 0 iso-date))
         (year (nth 2 iso-date)))
    (cons week year)))

(defun clocking/get-caption (orgblock week year)
  (let ((from (calendar-date-string
               (calendar-gregorian-from-absolute
                (calendar-iso-to-absolute (list week 1 year)))
               t t))
        (to (calendar-date-string
             (calendar-gregorian-from-absolute
              (calendar-iso-to-absolute (list week 0 year)))
             t t)))
    (format "#+CAPTION: Clock summary for week %s (%s - %s)\\n"
            orgblock from to)))

(defun clocking/org-clock-report (&optional arg)
  (interactive "P")
  (let* ((week-year (clocking/get-week-year arg))
         (week (car week-year))
         (year (cdr week-year))
         (heading (format "W%02d" week))
         (orgblock (format "%d-W%02d" year week))
         (caption (clocking/get-caption orgblock week year))
         (org-link-search-inhibit-query t))
    (save-excursion
      (condition-case nil
          (progn
            (org-link-search (format "#%s" orgblock) nil nil nil)
            (org-next-block 1))
        (error                        ; no match
         (org-link-search (format "*%d" year) nil nil nil)
         (org-end-of-subtree)
         (outline-previous-heading)
         (org-insert-heading)
         (insert heading)
         (org-clock-remove-overlays)
         (let ((props (list :name "clocktable"
                            :maxlevel 3
                            :block (intern orgblock)
                            :header caption)))
           (org-create-dblock
            (org-combine-plists org-clock-clocktable-default-properties props)))
         (when (search-forward "\\\\n")
           (replace-match "\\\\n"))
         (org-set-property "EXPORT_OPTIONS" "toc:nil")
         (org-set-property "CUSTOM_ID" orgblock)
         (org-set-tags-to '("due"))))
      (org-dblock-update)
      (outline-next-heading)
      (org-next-block 1)
      (org-dblock-update))))

(defun update-clocktimes-and-push (&optional arg)
  (interactive "P")
  (if (not (magit-everything-clean-p))
      (progn
        (magit-stage-all)
        (magit-commit)
        (message "Run the function again after committing.")
        (sleep-for 2))
    (clocking/org-clock-report arg)
    (save-buffer)
    (magit-stage-all)
    (magit-run-git "commit" "-m" "clock table update")
    ; (magit-push)
    ))

(provide 'clocking)
