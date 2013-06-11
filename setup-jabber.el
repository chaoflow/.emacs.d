(require 'jabber-autoloads
         (concat site-lisp-dir "/emacs-jabber/jabber-autoloads") t)

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

     (add-to-list 'jabber-invalid-certificate-servers "chaoflow.net")
     (add-to-list 'gnutls-trustfiles "/etc/ssl/certs/ca-bundle.crt")))
(provide 'setup-jabber)
