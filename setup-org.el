(defun myorg-update-parent-cookie ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))

(defadvice org-kill-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

;; Ensure the Latest Org-mode manual is in the info directory
(if (boundp 'Info-directory-list)
    (setq Info-directory-list (append Info-directory-list
                                      Info-default-directory-list))
  (setq Info-directory-list Info-default-directory-list))
(setq Info-directory-list
      (cons (expand-file-name
             "doc"
             (expand-file-name
              "org-mode"
              (expand-file-name "site-lisp" user-emacs-directory)))
            Info-directory-list))

(provide 'setup-org)
