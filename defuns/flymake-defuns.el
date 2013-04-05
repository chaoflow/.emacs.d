(defvar python-codechecker "flake8"
  "codechecker for flymake in python-mode")

(defun jho/flymake-pycodecheck-find-checker ()
  "Check for a project local python-codechecker. Else assume it is in PATH."
  (let* ((proj-dir (locate-dominating-file buffer-file-name "nix.env"))
         (checker (expand-file-name (concat "bin/" python-codechecker) proj-dir)))
    (if (file-executable-p checker)
        checker
      python-codechecker)))

(defun dss/flymake-pycodecheck-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list (jho/flymake-pycodecheck-find-checker)
          (list local-file))))

(defun dss/pylint-msgid-at-point ()
  (interactive)
  (let (msgid
        (line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info msgid)
      (if (eq (car elem) line-no)
            (let ((err (car (second elem))))
              (setq msgid (second (split-string (flymake-ler-text err)))))))))

(defun dss/pylint-silence (msgid)
  "Add a special pylint comment to silence a particular warning."
  (interactive (list (read-from-minibuffer "msgid: " (dss/pylint-msgid-at-point))))
  (save-excursion
    (comment-dwim nil)
    (if (looking-at "pylint:")
        (progn (end-of-line)
               (insert ","))
        (insert "pylint: disable-msg="))
    (insert msgid)))

(defun turn-on-flymake ()
  "Activating the flymake minor mode"
  (flymake-mode 1))
