(require 'notmuch)

(global-set-key [f12] 'notmuch)
(global-set-key [(shift f12)] 'notmuch-unread)

(setq user-mail-address "coroa@online.de"
      user-full-name "Jonas Hörsch"
      smtp-accounts
      '(("coroa@online.de" "Jonas Hörsch" "smtp.gmail.com" "coroan" "submission")
        ("jonas.hoersch@uni-potsdam.de" "Jonas Hörsch" "smtpout.uni-potsdam.de" nil "smtp")
        ("jonas.hoersch@fu-berlin.de" "Jonas Hörsch" "mail.zedat.fu-berlin.de" nil "submission")
        ("coroa@physik.fu-berlin.de" "Jonas Hörsch" "mail.zedat.fu-berlin.de" nil "submission")
        ("jonas.hoersch@univie.ac.at" "Jonas Hörsch" "mail.univie.ac.at" nil "submission")
        ("bluecherwg@gmail.com" "Jonas" "smtp.gmail.com" "bluecherwg" "submission")))


;;; sign all messages by default
(add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)


;;; org-mime
(require 'org-mime (concat site-lisp-dir "/org-mode/contrib/lisp/org-mime.el"))
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
