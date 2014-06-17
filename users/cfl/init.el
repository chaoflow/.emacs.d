;; notmuch
(require 'gnus-art)
(require 'notmuch)
(add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)

(define-key notmuch-search-mode-map "d"
  (lambda ()
    "toggle deleted tag for message"
    (interactive)
    (notmuch-search-tag
     (if (member "deleted" (notmuch-show-get-tags))
         "-deleted" "+deleted"))))


;; org
(add-to-list 'load-path (concat site-lisp-dir "/org-mode/contrib/lisp"))
(require 'org-notmuch)

;; XXX: turned off until we have something that handles multiple
;; languages
(add-hook 'org-mode-hook 'flyspell-mode-off)

(setq-default
 org-capture-templates
 '(
   ("b" "buy" entry (file+headline org-default-notes-file "Notes to buy something")
    "* %? :buy:\n  %i\n  %a")
   ("e" "Event" entry (file+headline org-default-notes-file "Events")
    "* %? %^T\n  %i\n  %a")
   ("o" "Event" entry (file+headline "~/office/calendar.org" "Events")
    "* %? %^T\n  %i\n  %a")
   ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org.gpg"))
    "* %?\nEntered on %U\n  %i\n  %a")
   ("0" "0x2c change" entry (file+datetree "/ssh:0x2c.org:/home/docs/0x2c-changelog.org")
    "* %?\nEntered on %U by [[cf1@0x2c.org][chaoflow]]\n  %i\n  %a")
   ("m" "Meeting" entry (file+headline org-default-notes-file "Meetings")
    "* %? :meeting:\n  %i\n  %a")
   ("t" "Task" entry (file+headline org-default-notes-file "Tasks")
    "* TODO %?\n  %i\n  %a")
   ))
(setq-default org-default-notes-file (concat org-directory "/_.org"))
(setq-default org-log-done t)
(setq-default org-startup-indented t)
(setq-default org-refile-targets (quote ((org-agenda-files :maxlevel . 3))))
(setq-default org-refile-use-outline-path nil)
(setq-default org-outline-path-complete-in-steps nil)
(setq-default org-refile-allow-creating-parent-nodes (quote confirm))
;(setq-default org-completion-use-ido t)
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'bh/verify-refile-target)


(setq-default org-todo-keywords '((sequence "TODO" "NEXT" "DONE")))


;; XXX: This should be separate from the variable that can be
;; customized and both together should form `org-agenda-files`
;; add .org files of all projects to agenda-files
;;(setq-default org-agenda-files)

;; (mapc
;;  (lambda (f)
;;    (add-to-list 'org-agenda-files f t)
;;    )
;;  (directory-files "~/projects/" t "^[^.].+")
;;  )

;; see https://github.com/kiwanami/emacs-calfw#readme
;;(require 'calfw-org)
;; First day of the week
(setq calendar-week-start-day 1) ; 0:Sunday, 1:Monday
(setq cfw:render-line-breaker 'cfw:render-line-breaker-wordwrap)


; Set Emacs' Default Browser to Conkeror
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "conkeror")


(global-rainbow-delimiters-mode)


(require 'whitespace)
(eval-after-load 'js2-mode
  '(progn
     (add-hook 'js2-mode-hook 'whitespace-mode)
     (add-to-list 'whitespace-style (quote indentation::space))))


(defun my-html-mode-hook ()
  (setq-local whitespace-style (quote (face trailing empty indentation))))

;(add-hook 'html-mode-hook 'adaptive-wrap-prefix-mode)
;(add-hook 'html-mode-hook 'org-indent-mode)
(add-hook 'html-mode-hook 'whitespace-mode)
(add-hook 'html-mode-hook 'my-html-mode-hook)
(add-hook 'html-mode-hook 'turn-off-auto-fill)


(setq ac-auto-start nil)

;;; paste at point instead of under mouse pointer
(setq mouse-yank-at-point t)


;;; owncloud calendar synchronization

;;; synchronization is set into motion calling M-x org-caldav-sync.

;;; every heading which is changed or deleted due to a change in
;;; owncloud is appended to ~/.emacs.d/org-caldav-backup.org

;;; WARNING: this will add an uuid4 to EVERY heading in all files from
;;; :files
;;; also refer to documentation at https://github.com/coroa/org-caldav


(require 'org-caldav)

(setq org-caldav-url "https://0x2c.org/radicale"
      ;; increase debugging output in *org-caldav-debug* buffer
      org-caldav-debug-level 2
      org-caldav-calendars
      '(
        (:calendar-id "chaoflownet/office"
                      :files ("~/org/office.org")
                      :inbox (file+headline "~/org/office.org"
                                            "INBOX")
                      ;; :select-tags ("office")
                      ;; to narrow to specific tags
                      )
        (:calendar-id "flo/personal"
                      :files ("~/org/personal.org")
                      :inbox (file+headline "~/org/personal.org"
                                            "INBOX")
                      ;; :select-tags ("personal")
                      )
      ))


(require 'znc)

(setq debug-on-error t)

(global-set-key
 "\C-xm"
 (lambda ()
   (interactive)
   (load-library (symbol-name major-mode))
   (execute-extended-command "" (symbol-name major-mode))))


;;; sprunge
(defun sprunge (prefix)
  "Posts the current buffer to sprunge, and shows the resulting URL in a new buffer"
  (interactive "P")
  (let ((filename "/tmp/sprunge-post"))
    (if prefix (write-file filename) (write-region (region-beginning) (region-end) filename)) ; if invoked with the universal argument / prefix, upload the whole file, else upload just the region
    (insert (shell-command-to-string (concat "curl -s -F 'sprunge=<" filename "' http://sprunge.us")))
    (delete-char -1))) ; Newline after URL


