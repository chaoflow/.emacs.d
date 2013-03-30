(require 'notmuch)

(defun notmuch-unread ()
  (interactive)
  (notmuch-search "tag:unread"))

(global-set-key [f12] 'notmuch)
(global-set-key [(shift f12)] 'notmuch-unread)

(setq notmuch-search-oldest-first nil
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-specify-envelope-from 'header
      notmuch-show-all-multipart/alternative-parts nil)

;;; Tagging keybindings

(define-key notmuch-show-mode-map "S"
  (lambda ()
    "mark message as spam"
    (interactive)
    (notmuch-show-tag-message "+spam" "-inbox")))

(define-key notmuch-search-mode-map "S"
  (lambda ()
    "mark messages in thread as spam"
    (interactive)
    (notmuch-search-tag "+spam -inbox")))

(define-key notmuch-show-mode-map "d"
      (lambda ()
        "toggle deleted tag for message"
        (interactive)
        (notmuch-show-tag-message
          (if (member "deleted" (notmuch-show-get-tags))
              "-deleted" "+deleted"))))

(define-key notmuch-search-mode-map "d"
      (lambda ()
        "toggle deleted tag for message"
        (interactive)
        (notmuch-search-tag
          (if (member "deleted" (notmuch-search-get-tags))
              "-deleted" "+deleted"))))


;;; Faces
(setq notmuch-search-line-faces '(("unread" :weight bold)
                                  ("flagged" :foreground "red")))

(setq mm-discouraged-alternatives '("text/html" "text/richtext")
      mm-decrypt-option 'known)

;;; extra minor modes

(defun turn-on-epa-mail-mode ()
  (epa-mail-mode 1))

(add-hook 'notmuch-show-hook 'turn-on-orgstruct-mode)
(add-hook 'notmuch-show-hook 'turn-on-epa-mail-mode)

;;; org-notmuch

(require 'org-notmuch (concat site-lisp-dir "/org-mode/contrib/lisp/org-notmuch.el"))

;;; writing/sending mail

;; multiplesmtpaccounts
;; http://emacswiki.org/emacs/MultipleSMTPAccounts

(require 'smtpmail)
(require 'network-stream)

(setq smtp-accounts
      '(("coroa@online.de" "Jonas Hörsch" "smtp.gmail.com" "coroan" "submission")
        ("jonas.hoersch@uni-potsdam.de" "Jonas Hörsch" "smtpout.uni-potsdam.de" nil "smtp")
        ("jonas.hoersch@fu-berlin.de" "Jonas Hörsch" "mail.zedat.fu-berlin.de" nil "submission")
        ("coroa@physik.fu-berlin.de" "Jonas Hörsch" "mail.zedat.fu-berlin.de" nil "submission")
        ("jonas.hoersch@univie.ac.at" "Jonas Hörsch" "mail.univie.ac.at" nil "submission")
        ("bluecherwg@gmail.com" "Jonas" "smtp.gmail.com" "bluecherwg" "submission")))

(defun my-change-smtp ()
  (save-excursion
    (loop with from = (save-restriction
                        (message-narrow-to-headers)
                        (message-fetch-field "from"))
          for (addr fname server user service) in smtp-accounts
          when (string-match addr from)
          do (setq user-mail-address addr
                   user-full-name fname
                   smtpmail-smtp-server server
                   smtpmail-smtp-user user
                   smtpmail-smtp-service service))))

(defadvice smtpmail-via-smtp
  (before change-smtp-by-message-from-field (recipient buffer &optional ask) activate)
  (with-current-buffer buffer (my-change-smtp)))

(setq user-mail-address "coroa@online.de"
      user-full-name "Jonas Hörsch"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service "submission"
      smtpmail-stream-type 'starttls
      smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
      message-send-mail-function 'smtpmail-send-it
      mml2015-sign-with-sender t
      message-citation-line-function 'message-insert-formatted-citation-line)

(add-hook 'message-mode-hook 'turn-on-orgstruct-mode)


;;; org-mime
(require 'org-mime (concat site-lisp-dir "/org-mode/contrib/lisp/org-mime"))
(setq org-mime-library 'mml)
(add-hook 'message-mode-hook
          (lambda () (local-set-key "\C-c\M-o" 'org-mime-htmlize)))
(add-hook 'org-mode-hook
          (lambda () (local-set-key "\C-c\M-o" 'org-mime-org-buffer-htmlize)))


;;; bbdb
(require 'bbdb-loaddefs (concat site-lisp-dir "/bbdb/lisp/bbdb-loaddefs.el"))

(setq bbdb-user-mail-address-re "coroa|hoersch@uni"
      bbdb-file "~/notes/.bbdb"
      bbdb-auto-revert t
      bbdb-check-auto-save-file t
      bbdb-expand-mail-aliases t
      bbdb-complete-mail-allow-cycling t
      bbdb-phone-style nil
      bbdb-pop-up-window-size 10)

(bbdb-initialize 'gnus 'message)

(provide 'user-notmuch)
