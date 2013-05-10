(add-to-list 'load-path (concat site-lisp-dir "/org-mode/lisp"))
(require 'org)

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
(add-to-list 'Info-directory-list
             (concat site-lisp-dir "/org-mode/doc"))

(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-startup-indented t)

(provide 'setup-org)
