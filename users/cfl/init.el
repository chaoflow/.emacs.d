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

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


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


(require 'znc)
