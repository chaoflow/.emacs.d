;;; theme
(add-to-list 'custom-theme-load-path user-settings-dir)
(setq custom-safe-themes '("4742e7b5664c09989997119e29b0e07198659e4f2e8484a5cd7c11a39ab102e1" default))
(load-theme 'tango-dark-better-highlight)

(eval-after-load 'setup-magit
  '(progn
     (set-face-attribute 'magit-item-highlight nil :background "#414a4d")
     (set-face-attribute 'magit-diff-none nil :inherit 'none)))

;;; diff extras
(setq diff-switches "-u")

;;; ffap
(require 'ffap)

;;; magit
(global-set-key (kbd "C-x m") 'magit-status)


;;; rgrep
(global-set-key (kbd "C-x C-r") 'rgrep)


;;; font size changes
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)


;;; occur inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))


;; ido
(setq ido-use-filename-at-point t
      ido-default-file-method 'selected-window
      ido-case-fold t)


;;; blinking cursors suck
(blink-cursor-mode 0)


;;; coding hook
(require 'coding-hook)


;;; folding
(defun jao-toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (if selective-display nil (or column (current-column)))))

(add-hook 'coding-hook (lambda () (local-set-key (kbd "C-c f") 'jao-toggle-selective-display)))


;;; comment-auto-fill
(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(add-hook 'coding-hook 'local-comment-auto-fill)


;;; whitespace
(require 'whitespace)
(setq whitespace-style '(face trailing))

;; (setq whitespace-style '(face indentation::space trailing))

(defun turn-on-whitespace ()
  (whitespace-mode t))
(add-hook 'coding-hook 'turn-on-whitespace)


;;; org mode
(require 'user-org)


;;; coding style
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "gnu")))


;;; PHP
(add-to-list 'auto-mode-alist '("\.php[34]?$" . php-mode))
(autoload 'php-mode "php-mode" "PHP editing mode." t)


;;; hungry delete
(global-set-key "" (quote hungry-delete-forward))
(global-set-key "" (quote hungry-delete-backward))


;;; windmove
(windmove-default-keybindings)


;;; default browser
(setq browse-url-generic-program "conkeror"
      browse-url-browser-function '(("^http:" . browse-url-generic)
                                    ("^https:" . browse-url-generic)
                                    ("^mailto:" . browse-url-mail)
                                    ("." . browse-url-generic)))

;;; latex
(add-hook 'LaTeX-mode-hook
 (lambda()
   (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
   (add-to-list 'TeX-command-list '("LaTeX SE" "%`%l%(mode) -shell-escape %' %t" TeX-run-TeX nil t))
   (setq TeX-save-query nil)))


;; (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)


;;; ido vertical
(setq ido-decorations  '("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))
(defun ido-disable-line-truncation ()
  (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

;;; ido auto searching on C-c C-s instead of automatically
(setq ido-auto-merge-delay-time 99999)
(define-key ido-file-dir-completion-map (kbd "C-c C-s")
  (lambda()
    (interactive)
    (ido-initiate-auto-merge (current-buffer))))

;;; just show don't try to raise (because raising often doesn't work)
(setq ido-default-buffer-method 'selected-window)


;;; ERC Config
(setq erc-email-userid "coroa@online.de"
      erc-nick (user-login-name)
      erc-user-full-name "Jonas"
      erc-server "irc.freenode.net"
      erc-fill-function 'erc-fill-variable
      ;; erc-autojoin-channels-alist '(("freenode.net" "#bewelcome" "#patterns"))
      erc-hide-list '("JOIN" "NICK" "PART" "QUIT" "MODE"))
      ;; #1987.4

(add-hook 'erc-echo-notice-hook 'erc-echo-notice-in-minibuffer)

(defun erc-pumuckl ()
  "Connect to ZNC on Pumuckl."
  (interactive)
  (erc-tls :server "pumuckl.fsr.physik.uni-potsdam.de"
           :port 669
           :nick "coroa"
           :full-name "Jonas"))


;;; notmuch
(require 'user-notmuch)

;;; gnus
(require 'user-gnus)

;;; python
(setq python-codechecker "flake8-python2")

;;; ibuffer lift
(require 'ibuf-ext)
(setq ibuffer-saved-filter-groups
      '(("default"
         ("dired" (mode . dired-mode))
         ("erc" (mode . erc-mode))
         ("chaoflow" (or (filename . "projects")
                         (name . "^chaoflow\\.")
                         (filename . ".emacs.d/src/org-sync")))
         ("emacs-config" (or (filename . ".emacs.d")))
         ("code" (filename . "code"))
         ("web dev" (or (mode . html-mode)
                        (mode . css-mode)))
         ("subversion" (name . "\*svn"))
         ("magit" (name . "\*magit"))
         ("org" (or (mode . org-mode)
                    (mode . org-agenda-mode)
                    (filename . "OrgMode")))
         ("gnus/notmuch" (or
                          (mode . message-mode)
                          (mode . bbdb-mode)
                          (mode . mail-mode)
                          (mode . gnus-group-mode)
                          (mode . gnus-summary-mode)
                          (mode . gnus-article-mode)
                          (mode . notmuch-hello-mode)
                          (mode . notmuch-show-mode)
                          (mode . notmuch-search-mode)
                          (name . "^\\.bbdb$")
                          (name . "^\\.newsrc-dribble")
                          (name . "^\\*OfflineIMAP\\*$")
                          (name . "^\\*.*imap")
                          (name . "^\\*gnus")))
         ("help" (or
                  (name . "\*Help\*")
                  (name . "\*Apropos\*")
                  (name . "\*info\*")))
         ("emacs" (or
                   (name . "^\\*scratch\\*$")
                   (name . "^\\*Messages\\*$")
                   (name . "^\\*")))))
      ibuffer-show-empty-filter-groups nil)

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

;;; less smooth scrolling
(setq smooth-scroll-margin 5)

;;; sprunge
(defun sprunge (prefix)
  "Posts the current buffer to sprunge, and shows the resulting URL in a new buffer"
  (interactive "P")
  (let ((filename "/tmp/sprunge-post"))
    (if prefix (write-file filename) (write-region (region-beginning) (region-end) filename)) ; if invoked with the universal argument / prefix, upload the whole file, else upload just the region
    (insert (shell-command-to-string (concat "curl -s -F 'sprunge=<" filename "' http://sprunge.us")))
    (delete-char -1))) ; Newline after URL


;;; small helper for pushing due

(defun push-due-to-0x2c ()
  (interactive)
  (with-current-buffer
      (with-current-buffer
          (find-file-noselect (expand-file-name "~/notes/chaoflow.org"))
        (org-block-map (lambda ()
                         (let ((tags (nth 5 (org-heading-components))))
                           (when (and (stringp tags) (string-match ":due:" tags)) (org-dblock-update)))))

        (let ((org-export-select-tags '("due")))
          (save-window-excursion
            (org-ascii-export-as-ascii nil nil nil t))))
    (write-file "/0x2c.org:due.txt")
    (kill-buffer)))

;;; session restart
(setq desktop-load-locked-desktop t)
(desktop-save-mode 1)


;;; require ergonomic keybindings
(require 'ergonomic-keybindings)
