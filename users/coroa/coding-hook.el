(defvar coding-hook nil
  "Hook that gets run on activation of any programming mode.")

(defun run-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'coding-hook))

(eval-after-load 'c-mode '(add-hook 'c-mode-hook 'run-coding-hook))
(eval-after-load 'python '(add-hook 'python-mode-hook 'run-coding-hook))
(eval-after-load 'lisp-mode
  '(progn
     (add-hook 'lisp-mode-hook 'run-coding-hook)
     (add-hook 'emacs-lisp-mode-hook 'run-coding-hook)))
(eval-after-load 'js2-mode '(add-hook 'js2-mode-hook 'run-coding-hook))
(eval-after-load 'sgml-mode '(add-hook 'sgml-mode-hook 'run-coding-hook))

;; (eval-after-load 'clojure-mode '(add-hook 'clojure-mode-hook 'run-coding-hook))
;; (eval-after-load 'scheme-mode '(add-hook 'scheme-mode-hook 'run-coding-hook))
;; (eval-after-load 'ruby-mode '(add-hook 'ruby-mode-hook 'run-coding-hook))

(provide 'coding-hook)

