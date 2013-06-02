(defcustom cycle-themes '(tango-dark tango)
  "List of themes through which one can switch easily by calling
`cycle-themes' repeatedly. The first element is loaded at
startup."
  :type '(repeat symbol))

(defvar cycle-current-theme nil
  "Used internally to hold a pointer to the currently loaded theme.")

(defun cycle-themes ()
  "Cycle through themes from the variable cycle-themes."
  (interactive)
  (let ((next-theme (car (or (cdr (memq cycle-current-theme cycle-themes))
                             cycle-themes))))
    (when next-theme
     (when (custom-theme-enabled-p cycle-current-theme)
       (disable-theme cycle-current-theme))
     (when (load-theme next-theme t)
       (setq cycle-current-theme next-theme)))))

(provide 'setup-themes)
