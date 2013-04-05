(require 'python)

(add-hook 'python-mode-hook 'whitespace-mode)

;;; virtualenv

(autoload 'virtualenv-activate "virtualenv"
  "Activate a Virtual Environment specified by PATH" t)
(autoload 'virtualenv-workon "virtualenv"
  "Activate a Virtual Environment present using virtualenvwrapper" t)

(define-key python-mode-map (kbd "C-m") 'newline-and-indent)

;;; flymake
(when (require 'flymake nil t)
  (require 'flymake-cursor nil t)

  (add-to-list 'flymake-allowed-file-name-masks
               (list "\\.py\\'" 'dss/flymake-pycodecheck-init))

  (define-key python-mode-map (kbd "C-c C-n") 'flymake-goto-next-error)
  (define-key python-mode-map (kbd "C-c C-p") 'flymake-goto-prev-error)

  (setq flymake-no-changes-timeout 30
        flymake-start-syntax-check-on-newline t)

  (add-hook 'python-mode-hook 'turn-on-flymake))

;;; ipython
(let ((ipython (or (executable-find "ipython2")
                   (executable-find "ipython"))))
  (when ipython
    (setq python-shell-interpreter (file-name-nondirectory ipython)
          python-shell-interpreter-args ""
          python-shell-prompt-regexp "In \\[[0-9]+\\]: "
          python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
          python-shell-completion-setup-code
          "from IPython.core.completerlib import module_completion"
          python-shell-completion-module-string-code
          "';'.join(module_completion('''%s'''))\n"
          python-shell-completion-string-code
          "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")))

(provide 'setup-python)
