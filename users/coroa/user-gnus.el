(setq gnus-directory "~/.gnus/"
      gnus-cache-directory (concat gnus-directory "cache/")
      gnus-kill-files-directory gnus-directory
      message-directory (concat gnus-directory "Mail/")
      nnml-directory (concat gnus-directory "Mail/")
      nndraft-directory (concat gnus-directory "Mail/drafts/")
      message-auto-save-directory (concat gnus-directory "Mail/drafts/")
      gnus-init-file (concat user-settings-dir "/dot-gnus.el"))

(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]")

(require 'gnus)
(global-set-key [f11] 'gnus)
(setq mail-user-agent 'gnus-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
(setq gnus-activate-level 2)

(setq gnus-read-newsrc-file nil
      gnus-save-newsrc-file nil
      gnus-save-killed-list nil)

(setq gnus-update-message-archive-method t
      gnus-message-archive-method
      '(nnfolder "archive"
                 (nnfolder-directory   "~/.gnus/Mail/archive")
                 (nnfolder-active-file "~/.gnus/Mail/archive/active")
                 (nnfolder-get-new-mail nil)
                 (nnfolder-inhibit-expiry t)))

(setq gnus-select-method '(nntp "news.gmane.org"
                                (nntp-open-connection-function nntp-open-tls-stream)
                                (nntp-port-number 563)))
(add-to-list 'gnus-secondary-select-methods
             '(nntp "news1.open-news-network.org"
                    (nntp-open-connection-function nntp-open-tls-stream)
                    (nntp-port-number 563)))
(add-to-list 'gnus-secondary-select-methods
             '(nntp "news.gwene.org"
                    (nntp-open-connection-function nntp-open-tls-stream)
                    (nntp-port-number 563)))

(setq gnus-agent t
      gnus-agent-go-online 'always)

(setq gnus-keep-same-level 'best
      gnus-group-use-permanent-levels t
      gnus-ignored-from-addresses
      (regexp-opt '("jonas.hoersch@uni-potsdam.de"
                    "hoersch@uni-potsdam.de"
                    "coroa@online.de"
                    "jonas.hoersch@univie.ac.at"
                    "jonas.hoersch@fu-berlin.de")))

(setq gnus-group-line-format "%M%S%p%P%5y:%B%(%-40G%)  %s\n"
      gnus-group-jump-to-group-prompt "nnimap+dovecot:")

(setq gnus-buttonized-mime-types '("multipart/signed" "application/pgp-encrypted"))


;;; summary view
;; as in
;; http://groups.google.com/group/gnu.emacs.gnus/browse_thread/thread/a673a74356e7141f


(setq gnus-sum-thread-tree-indent "  ")
(setq gnus-sum-thread-tree-root "") ;; "● ")
(setq gnus-sum-thread-tree-false-root "") ;; "◯ ")
(setq gnus-sum-thread-tree-single-indent "") ;; "◎ ")
(setq gnus-sum-thread-tree-vertical        "│")
(setq gnus-sum-thread-tree-leaf-with-other "├─► ")
(setq gnus-sum-thread-tree-single-leaf     "╰─► ")

(setq gnus-summary-line-format
      (concat
       "%0{%U%R%z%}"
       "  " "%{%B%}"
       "%1{%d%}" ;; date
       "  "
       "%4{%-23,23uB%}"               ;; name
       "  "
       "%(%s%)\n")
      gnus-summary-display-arrow t)
    
;;;; default
;; (setq gnus-summary-line-format "%U%R%z%I%(%[%4L: %-23,23f%]%) %s\n")
  
(setq gnus-parameters
      '(("nntp.*gwene\\..*"
         (gnus-summary-line-format "%U %d  %(%S%)\n")
         (gnus-show-threads nil))))


(setq gnus-message-archive-group 'identity
      gnus-gcc-mark-as-read t)

(setq gnus-posting-styles
      '((".*"
         (address "coroa@online.de")
         (name "Jonas Hörsch"))
        (":Univie"
         (address "jonas.hoersch@univie.ac.at")
         (name "Jonas Hörsch"))
        (":UP"
         (address "jonas.hoersch@uni-potsdam.de")
         (name "Jonas Hörsch"))
        (":FU"
         (address "jonas.hoersch@fu-berlin.de")
         (name "Jonas Hörsch"))
        ))

(setq gnus-completing-read-function 'gnus-ido-completing-read)  

(provide 'user-gnus)
