(require 'package)

(defvar marmalade '("marmalade" . "http://marmalade-repo.org/packages/"))
(defvar gnu '("gnu" . "http://elpa.gnu.org/packages/"))
(defvar melpa '("melpa" . "http://melpa.milkbox.net/packages/"))

(setq-default package-user-dir (concat user-emacs-directory "elpa"))

;; Add marmalade to package repos
(add-to-list 'package-archives marmalade)
(add-to-list 'package-archives melpa t)

;; Add packages defined elsewhere as builtin packages, so they will
;; not be pulled in a second time
(eval-after-load 'finder-inf
  '(setq package--builtins
	 (nconc '(
		  (yasnippet . [(0 8) nil "yasnippet"])
               ;; (pkgname . [(maj-ver min-ver) nil "description"])
		  ) package--builtins)))

(package-initialize)

(unless (and (file-exists-p (concat package-user-dir "/archives/marmalade"))
             (file-exists-p  (concat package-user-dir "/archives/gnu"))
             (file-exists-p (concat package-user-dir "/archives/melpa")))
  (package-refresh-contents))

(defun packages-install (&rest packages)
  (mapc (lambda (package)
          (let ((name (car package))
                (repo (cdr package)))
            (when (not (package-installed-p name))
              (let ((package-archives (list repo)))
                (package-initialize)
                (package-install name)))))
        packages)
  (package-initialize)
  (delete-other-windows))

(provide 'setup-package)
