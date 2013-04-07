;;; py-project-root helpers

(defvar py-project-root 'not-initialized
  "The root of the project the current buffer is in.")
(make-variable-buffer-local 'py-project-root)

(defun py-project-find-root ()
  "Find an appropriate project root for the current buffer.

If no root directory is found, nil is returned."
  (directory-file-name
   (locate-dominating-file buffer-file-name "dev.nix")))

(defun py-project-root ()
  "Return the root of the current buffer's project.

You can set the variable `py-project-root' in, for example,
.dir-locals.el to configure this."
  (when (eq py-project-root 'not-initialized)
    ;; Set it to nil so when the user runs C-g on the project root
    ;; prompt, it's set to "no project root".
    (setq py-project-root nil)
    (setq py-project-root
          ;; (or
          (py-project-find-root)
          ;; (read-directory-name "Project root: "
          ;;                      default-directory))
          )
    ;; (when (and (not (file-directory-p py-project-root))
    ;;            (y-or-n-p "Directory does not exist, create? "))
    ;;   (make-directory py-project-root t))
    )
  py-project-root)


;;; ipython

(defun use-ipython-locally ()
  "Use ipython in the local buffer if it is available"
  (when (executable-find "ipython")
    (set (make-local-variable 'python-shell-interpreter)
         (file-name-nondirectory "ipython"))
    (set (make-local-variable 'python-shell-interpreter-args)
         "")
    (set (make-local-variable 'python-shell-prompt-regexp)
         "In \\[[0-9]+\\]: ")
    (set (make-local-variable 'python-shell-prompt-output-regexp)
         "Out\\[[0-9]+\\]: ")
    (set (make-local-variable 'python-shell-completion-setup-code)
         "from IPython.core.completerlib import module_completion")
    (set (make-local-variable 'python-shell-completion-module-string-code)
         "';'.join(module_completion('''%s'''))\n")
    (set (make-local-variable 'python-shell-completion-string-code)
         "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")))


;;; flymake stuff

(defvar py-codechecker "flake8"
  "codechecker for flymake in python-mode")

(defun jho/flymake-pycodecheck-find-checker ()
  "Check for a project local python-codechecker. Else assume it is in PATH."
  (let ((checker (expand-file-name (concat "bin/" py-codechecker)
                                   (py-project-root))))
    (if (file-executable-p checker)
        checker
      (when (executable-find py-codechecker)
        py-codechecker))))

(defun dss/flymake-pycodecheck-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list (jho/flymake-pycodecheck-find-checker)
          (list local-file))))


;;;  helper

(defun py-rgrep-symbol (symbol)
  "Search for SYMBOL in the current project.

SYMBOL defaults to the symbol at point, or the current region if
active.

With a prefix argument, prompt for a string to search for."
  (interactive
   (list
    (cond
     (current-prefix-arg
      (read-from-minibuffer "Search for symbol: "))
     ((use-region-p)
      (buffer-substring-no-properties (region-beginning)
                                      (region-end)))
     (t
      (or (thing-at-point 'symbol)
          (read-from-minibuffer "Search for symbol: "))))))
  (grep-compute-defaults)
  (rgrep (format "\\b%s\\b" symbol)
         "*.py"
         (py-project-root))
  (with-current-buffer next-error-last-buffer
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^find .*" nil t)
          (replace-match (format "\\1\nSearching for symbol %s\n"
                                 symbol)))))))
