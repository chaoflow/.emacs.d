(require 'python)
(require 'cl-lib)

;;; elpy

(elpy-enable t)
(add-to-list 'python-mode-hook 'elpy-initialize-local-variables)
(elpy-use-ipython)

;; make elpy fail gracefully and show a warning, when a necessary
;; module is missing

(defalias 'elpy-installation-instructions
  #'(lambda (message &optional show-elpy-module)
      (message message)
      (ding)
      (sit-for 1)))

(defvar elpy-rpc-inhibit nil
  "If it is t, the function elpy-rpc will always just return nil")
(make-variable-buffer-local 'elpy-rpc-inhibit)

(defadvice elpy-rpc (around inhibitable activate)
  "Check if inhibited by elpy-rpc-inhibit"
  (unless elpy-rpc-inhibit
    ad-do-it))

(defadvice elpy-rpc-open (around fail-gracefully activate)
  (condition-case err
      ad-do-it
    (error
     (setq elpy-rpc-inhibit t)
     (error (concat (cadr err) "; inhibiting further rpc tries.")))))


(defadvice elpy-project-root (around silent-elpy-project-root activate)
  "Don't ask for a project-root. If it's not there, it's not there."
  (cl-letf (((symbol-function 'read-directory-name)
             #'(lambda (prompt &optional dir default-dirname
                          mustmatch initial) (signal 'quit nil))))
    (condition-case err
        ad-do-it
      (quit nil))))

(defadvice elpy-project-find-root
  (around prefer-dev-nix-elpy-project-find-root activate)
  "Look first whether there is a directory, which contains the file dev.nix"
  (let ((devnixroot (locate-dominating-file default-directory "dev.nix")))
    (if devnixroot
        (setq ad-return-value devnixroot)
      ad-do-it)))


;;; flymake

(eval-after-load 'flymake
  '(progn
     (require 'flymake-cursor nil t)

     (setq flymake-no-changes-timeout 30
           flymake-start-syntax-check-on-newline nil
           flymake-gui-warnings-enabled nil
           python-check-command "flake8")

     (set-face-attribute 'flymake-errline nil :inherit nil)
     (set-face-attribute 'flymake-warnline nil :inherit nil)))


;;; whitespace-mode

;;; deactivate highlight-indentation-mode
(cl-callf2 delq 'highlight-indentation-mode elpy-default-minor-modes)
(add-to-list 'elpy-default-minor-modes 'whitespace-mode)


;;; AutoComplete

(eval-after-load 'auto-complete
  '(progn
     (ac-set-trigger-key "<M-tab>")

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

(define-key python-mode-map (kbd "C-c C-h") 'pylookup-lookup)

(defun prepare-pylookup-in-nix-environment (src &optional buffer-local)
  "Select a pylookup database based on the nix hash of the python
html documentation in SRC. And create it if necessary."
  (let ((nix-hash (find-corresponding-nix-hash src)))
    (when nix-hash
      (setf (if buffer-local
                (symbol-value (make-local-variable 'pylookup-db-file))
              (default-value 'pylookup-db-file))
            (concat pylookup-dir "/pylookup-" nix-hash ".db"))
      (unless (file-exists-p pylookup-db-file)
        (pylookup-update (file-truename src)))
      pylookup-db-file)))

(eval-after-load 'pylookup
  '(prepare-pylookup-in-nix-environment
    (car (or (file-expand-wildcards "~/.nix-profile/share/doc/python*/html")
             (file-expand-wildcards "/run/current-system/sw/share/doc/python*/html")))))

(defun prepare-pylookup-in-buffer ()
  "Choose the pylookup database by the current projects nix profile."
  (when (elpy-project-root)
    (prepare-pylookup-in-nix-environment
     (car (file-expand-wildcards
           (concat (elpy-project-root)
                   "/nixprofile2.?/share/doc/python*/html")))
     t)))

(add-hook 'python-mode-hook 'prepare-pylookup-in-buffer)

;;; keybindings

(define-key python-mode-map (kbd "C-m") 'newline-and-indent)

;;; virtualenv

(defadvice compilation-start
  (around obey-buffer-local-environment-compilation-start activate)
  "Make compilation-start obey buffer local settings by let
binding the global symbols to the local values of the process
environment and the exec path, so that settings of any
environment variables are respected."
  (if (or (local-variable-p 'process-environment)
          (local-variable-p 'exec-path))
      (cl-letf (((default-value 'process-environment) process-environment)
                ((default-value 'exec-path) exec-path))
        ad-do-it)
    ad-do-it))

(defun py-switch-to-virtualenv ()
  "Switch to the virtualenv in the project root.

Needs to be the first hook to run."
  (when (elpy-project-root)
    (set (make-local-variable 'virtualenv-name) nil)
    (set (make-local-variable 'process-environment)
         (copy-sequence process-environment))
    (make-local-variable 'exec-path)
    (virtualenv-activate (directory-file-name (elpy-project-root)))
    (set (make-local-variable 'python-shell-virtualenv-path)
         (directory-file-name (elpy-project-root)))))

;; should be the last element to add to python-mode-hook, so it runs FIRST
(add-hook 'python-mode-hook 'py-switch-to-virtualenv)


(provide 'setup-python)
