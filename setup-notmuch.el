;;; notmuch general settings

(setq notmuch-search-oldest-first nil
      notmuch-show-all-multipart/alternative-parts nil
      notmuch-crypto-process-mime t
      mm-discouraged-alternatives '("text/html" "text/richtext")
      mm-decrypt-option 'known
      message-citation-line-function 'message-insert-formatted-citation-line)


;; multiplesmtpaccounts
;; also refer to http://emacswiki.org/emacs/MultipleSMTPAccounts

(defvar smtp-accounts nil
  "List of available SMTP Accounts

Upon sending a message via SMTP the entry which matches the message's from-header is used to set the smtp configuration.

An entry is a list of:
\(address full-name host user service\)

The corresponding credentials are looked for in ~/.authinfo.gpg.

Example:
'\(\(\"coroa@online.de\" \"Jonas Hörsch\" \"smtp.gmail.com\" \"coroan\" \"submission\"\)
  \(\"coroa@0x2c.org\" \"Jonas Hörsch\" \"smtp.0x2c.org\" \"coroa\" \"submission\"\)\)")

(defadvice smtpmail-via-smtp
  (around change-smtp-by-message-from-field (recipient buffer &optional ask) activate)
  (with-current-buffer buffer
    (loop with from = (save-excursion
                        (save-restriction
                          (message-narrow-to-headers)
                          (message-fetch-field "from")))
          for (addr fname server user service) in smtp-accounts
          when (string-match addr from)
          return (let ((user-mail-address addr)
                       (user-full-name fname)
                       (smtpmail-smtp-server server)
                       (smtpmail-smtp-user user)
                       (smtpmail-smtp-service service))
                   ad-do-it)
          finally do
          (error (format (concat "address '%s' doesn't match"
                                 " any entry from smtp-accounts.") from)))))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
      mail-specify-envelope-from 'header
      mml2015-sign-with-sender t
      mml2015-encrypt-to-self t)


;;; defuns

(defun notmuch-unread ()
  (interactive)
  (notmuch-search "tag:unread"))


;;; orgstruct in mails

(defun turn-on-orgstruct-mode ()
  (orgstruct-mode t)
  (set (make-local-variable 'org-footnote-auto-label) 'plain)
  (local-set-key (kbd "C-c f") 'org-footnote-action))

(add-hook 'notmuch-show-hook 'turn-on-orgstruct-mode)
(add-hook 'message-mode-hook 'turn-on-orgstruct-mode)


;;; epa mail mode for decrypting inline PGP

(require 'epa-mail)

(defun turn-on-epa-mail-mode ()
  (epa-mail-mode 1))

(add-hook 'notmuch-show-hook 'turn-on-epa-mail-mode)
(add-hook 'message-mode-hook 'turn-on-epa-mail-mode)

(defun notmuch-show-decrypt-message ()
  (interactive)
  ;; make sure the content is not indented, as this confuses epa
  (when notmuch-show-indent-content
    (notmuch-show-toggle-thread-indentation))

  ;; and don't ask, i know what i want to do
  (cl-letf ((extent (notmuch-show-message-extent))
            ((symbol-function 'y-or-n-p) #'(lambda (msg) t)))
    (epa-decrypt-armor-in-region (car extent) (cdr extent))))

(defun use-notmuch-show-decrypt-message ()
  (make-local-variable 'epa-mail-mode-map)
  (define-key epa-mail-mode-map (kbd "C-c C-e d") 'notmuch-show-decrypt-message)
  (define-key epa-mail-mode-map (kbd "C-c C-e C-d") 'notmuch-show-decrypt-message))

(add-hook 'notmuch-show-hook 'use-notmuch-show-decrypt-message)


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

;;; refresh line at pos when returning to the search buffer

(defun notmuch-search-update-on-return-to-buffer ()
  (add-hook 'window-configuration-change-hook 'notmuch-search-update-tags nil t))

(add-hook 'notmuch-search-hook 'notmuch-search-update-on-return-to-buffer)

;;; org-notmuch

(require 'org-notmuch (concat site-lisp-dir "/org-mode/contrib/lisp/org-notmuch.el"))


;;; more visible faces for flagged, unread and deleted

(setq notmuch-search-line-faces '(("unread" :weight bold)
                                  ("flagged" :foreground "red")
                                  ("deleted" :foreground "gray60")))


(provide 'setup-notmuch)
