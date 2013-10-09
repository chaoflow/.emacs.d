(require 'jabber-autoloads
         (concat site-lisp-dir "/emacs-jabber/jabber-autoloads") t)

;;; dired-x's dired-jump binding C-x C-j conflicts with jabber
;;; it must be disabled BEFORE require'ing dired-x
(setq dired-bind-jump nil)

(eval-after-load 'jabber
  '(progn
     ;; history
     (setq jabber-history-enabled t
           ;; jabber-history-muc-enabled t
           jabber-use-global-history nil)

     ;; colour
     (setq jabber-muc-colorize-foreign t
           jabber-muc-colorize-local t)

     ;; roster
     (setq jabber-roster-show-title nil
           jabber-roster-show-bindings nil
           jabber-roster-line-format " %c %-25n %u %-8s  %S"
                                        ; no avatars
           jabber-roster-show-separators nil)

     (set-face-attribute 'jabber-title-large nil :height 1.2)
     (set-face-attribute 'jabber-title-medium nil :height 1.0)
     (set-face-attribute 'jabber-title-small nil :height 0.8)

     ;; reconnect
     (setq jabber-auto-reconnect t)

     (add-to-list 'jabber-invalid-certificate-servers "chaoflow.net")
     (add-to-list 'gnutls-trustfiles "/etc/ssl/certs/ca-bundle.crt")

     ;; jabber - muc names binding

     (defun bind-jabber-muc-names ()
       (when jabber-group
         (local-set-key (kbd "C-c C-n") 'jabber-muc-names)))
     (add-hook 'jabber-chat-mode-hook 'bind-jabber-muc-names)))

(eval-after-load 'jabber-activity
  '(progn
     ;; activity
     (setq jabber-activity-make-strings 'jabber-activity-make-strings-shorten)
     (set-face-attribute 'jabber-activity-personal-face nil :foreground "red")
     (set-face-attribute 'jabber-activity-face nil :foreground nil)))

(eval-after-load 'jabber-alert
  '(progn
     ;; alerts
     (setq jabber-alert-presence-hooks nil)
     (callf2 delq 'jabber-message-echo jabber-alert-message-hooks)
     (callf2 delq 'jabber-muc-echo jabber-alert-muc-hooks)))

(provide 'setup-jabber)
