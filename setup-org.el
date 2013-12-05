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
      org-startup-indented t
      org-log-into-drawer t
      org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;;; a few undisputed global keybindings every org'er will want

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(define-key org-mode-map (kbd "C-c f") 'org-footnote-action)

(provide 'setup-org)
