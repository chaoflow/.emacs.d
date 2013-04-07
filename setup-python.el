(require 'python)

;;; flymake

(when (require 'flymake nil t)
  (require 'flymake-cursor nil t)

  (add-to-list 'flymake-allowed-file-name-masks
               (list "\\.py\\'" 'dss/flymake-pycodecheck-init))

  (define-key python-mode-map (kbd "C-c C-n") 'flymake-goto-next-error)
  (define-key python-mode-map (kbd "C-c C-p") 'flymake-goto-prev-error)

  (setq flymake-no-changes-timeout 30
        flymake-start-syntax-check-on-newline nil
        flymake-gui-warnings-enabled nil
        py-codechecker "flake8")

  (set-face-attribute 'flymake-errline nil :underline 'unspecified)
  (set-face-attribute 'flymake-warnline nil :underline 'unspecified)

  (defun turn-on-flymake ()
    "Activating the flymake minor mode"
    (flymake-mode 1)

    (when (string-match "flake8" py-codechecker)
      (set (make-local-variable 'flymake-warning-re) "^W[0-9]")))

  (add-hook 'python-mode-hook 'turn-on-flymake))



;;; virtualenv

(autoload 'virtualenv-activate "virtualenv"
  "Activate a Virtual Environment specified by PATH" t)
(autoload 'virtualenv-workon "virtualenv"
  "Activate a Virtual Environment present using virtualenvwrapper" t)

(defun py-switch-to-virtualenv ()
  "Switch to the virtualenv in the project root.

Needs to be the first hook to run."
  (when (py-project-root)
    (make-local-variable 'process-environment)
    (make-local-variable 'exec-path)
    (set (make-local-variable 'virtualenv-name) nil)
    (virtualenv-activate (py-project-root))
    (set (make-local-variable 'python-shell-virtualenv-path)
         (py-project-root))))

(add-hook 'python-mode-hook 'py-switch-to-virtualenv)


;;; jedi
;;
;; C-.             jedi:goto-definition
;; <C-tab>         jedi:complete
;; C-c d           jedi:show-doc

(eval-when-compile (require 'jedi))
(setq jedi:setup-keys t
      jedi:key-complete (kbd "<M-tab>"))
(when (require 'jedi nil t)
  (setq jedi:get-in-function-call-delay 500
        jedi:tooltip-method nil)

  (defun turn-on-jedi ()
    (when (py-project-root)
      (jedi:ac-setup)

      (set (make-local-variable 'jedi:server-command)
           (list (expand-file-name "bin/python" (py-project-root))
                 jedi:server-script))
      (set (make-local-variable 'jedi:server-args)
           (list "--virtual-env" (py-project-root)))

      (auto-complete-mode t)
      (jedi-mode t))))

(add-hook 'python-mode-hook 'turn-on-jedi)

;;; ipython

(add-hook 'python-mode-hook 'use-ipython-locally)

;;; whitespace-mode

(add-hook 'python-mode-hook 'whitespace-mode)


;;; AutoComplete

(eval-after-load 'auto-complete
  '(progn
     ;; `ac-auto-show-menu': Short timeout because the menu is great.
     (setq ac-auto-show-menu nil
           ac-use-quick-help nil)

     ;; Fix some key bindings in ac completions. Using RET when a
     ;; completion is offered is not usually intended to complete (use
     ;; TAB for that), but done while typing and the inputer is considere
     ;; complete, with the intent to simply leave it as is and go to the
     ;; next line. Much like space will not complete, but leave it as is
     ;; and insert a space.
     (define-key ac-completing-map (kbd "RET") nil)
     (define-key ac-completing-map (kbd "<return>") nil)))


;;; pylookup

(eval-when-compile (require 'pylookup))
(setq pylookup-dir (expand-file-name "pylookup" site-lisp-dir))

(setq pylookup-program (concat pylookup-dir "/pylookup.py")
      pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; to speedup, just load it on demand
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)
(autoload 'pylookup-update "pylookup"
  "Run pylookup-update and create the database at `pylookup-db-file'." t)

(define-key python-mode-map (kbd "C-c h") 'pylookup-lookup)

;;; keybindings

(define-key python-mode-map (kbd "C-m") 'newline-and-indent)
(define-key python-mode-map (kbd "C-c C-s") 'py-rgrep-symbol)

(provide 'setup-python)
