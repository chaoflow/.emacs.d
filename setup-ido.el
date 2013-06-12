;; Interactively Do Things

(require 'ido)
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-case-fold nil
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-max-prospects 10)

(add-hook
 'ido-setup-hook
 (lambda ()
   ;; Go straight home
   (define-key ido-file-completion-map
     (kbd "~")
     (lambda ()
       (interactive)
       (cond
        ((looking-back "~/") (insert "projects/"))
        ((looking-back "/") (insert "~/"))
        (:else (call-interactively 'self-insert-command)))))

   ;; Use C-w to go back up a dir to better match normal usage of C-w
   ;; - insert current file name with C-x C-w instead.
   (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)
   (define-key ido-file-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name)))

;; Always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")

;; Use ido everywhere
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

(ido-ubiquitous-use-new-completing-read webjump 'webjump)
(ido-ubiquitous-use-new-completing-read yas-expand 'yasnippet)
(ido-ubiquitous-use-new-completing-read yas-visit-snippet-file 'yasnippet)

;; Fix ido-ubiquitous confusing completing-read compatibility handling

(defadvice ido-completing-read (before compensate-quirk activate)
  "Compensate for a quirk of `completing-read`.

From the `completing-read' docstring:

> If the input is null, `completing-read' returns DEF, or the
> first element of the list of default values, or an empty string
> if DEF is nil, regardless of the value of REQUIRE-MATCH.

This odd behavior is required for compatibility with an old-style
usage pattern whereby the default was requested by returning an
empty string. In this mode, the caller receives the empty string
and handles the default case manually, while `completing-read'
never has any knowledge of the default.

This usage pattern breaks with ido in the edge case when there is
no DEF and no INITIAL-INPUT because ido then returns the first
element on the list rather than the empty string. This advice
tries then to derive the default item from the prompt string by
matching against \"(default <string>)\" or, if there is no match,
it prepends the possible choices with the empty string, which
will thus be preselected."
  (when (and ido-next-call-replaces-completing-read
             (null def)
             (null initial-input))
    (if (string-match "(default \\(\"?\\)\\(.+\\)\\1)" prompt)
        (setq def (match-string 2 prompt))
      (push "" choices))))

(setq ido-ubiquitous-enable-compatibility nil)


(provide 'setup-ido)
