(require 'python)

(add-hook 'python-mode-hook 'whitespace-mode)

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

;;; virtualenv

(autoload 'virtualenv-activate "virtualenv"
  "Activate a Virtual Environment specified by PATH" t)
(autoload 'virtualenv-workon "virtualenv"
  "Activate a Virtual Environment present using virtualenvwrapper" t)

(define-key python-mode-map (kbd "C-m") 'newline-and-indent)

(defun setup-py-buffer ()
  "Check environment, activate available features"

  (set (make-local-variable 'py-project-directory)
       (locate-dominating-file buffer-file-name "dev.nix"))

  (when py-project-directory
    (setq py-project-directory
          (directory-file-name py-project-directory))

    (make-local-variable 'process-environment)
    (make-local-variable 'exec-path)
    (set (make-local-variable 'virtualenv-name) nil)
    (virtualenv-activate py-project-directory))

  (use-ipython-locally))

(add-hook 'python-mode-hook 'setup-py-buffer)

(provide 'setup-python)
