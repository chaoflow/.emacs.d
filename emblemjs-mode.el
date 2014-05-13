(eval-when-compile
  (defvar font-lock-beg)
  (defvar font-lock-end)
  (require 'cl))

;; User definable variables

(defgroup emblemjs nil
  "Support for the Emblemjs template language."
  :group 'languages
  :prefix "emblemjs-")

(defcustom emblemjs-mode-hook nil
  "Hook run when entering Emblemjs mode."
  :type 'hook
  :group 'emblemjs)

(defcustom emblemjs-indent-offset 2
  "Amount of offset per level of indentation."
  :type 'integer
  :group 'emblemjs)

(defcustom emblemjs-backspace-backdents-nesting t
  "Non-nil to have `emblemjs-electric-backspace' re-indent all code
nested beneath the backspaced line be re-indented along with the
line itself."
  :type 'boolean
  :group 'emblemjs)

(defvar emblemjs-indent-function 'emblemjs-indent-p
  "This function should look at the current line and return true
if the next line could be nested within this line.")

(defvar emblemjs-block-openers
  `("^ *\\([\\.#a-z][^ \t]*\\)\\(\\[.*\\]\\)?"
    "^ *[-=].*do[ \t]*\\(|.*|[ \t]*\\)?$"
    ,(concat "^ *-[ \t]*\\("
             (regexp-opt '("if" "unless" "while" "until" "else"
                           "begin" "elsif" "rescue" "ensure" "when"))
             "\\)")
    "^ *|"
    "^ */"
    "^ *[a-z0-9_]:")
  "A list of regexps that match lines of Emblemjs that could have
text nested beneath them.")

;; Font lock

;; Helper for nested block (comment, embedded, text)
(defun emblemjs-nested-re (re)
  (concat "^\\( *\\)" re "\n\\(?:\\(?:\\1 .*\\)\n\\)*"))

(defvar emblemjs-html5-tags
  '("a" "abbr" "acronym" "address" "applet" "area" "article" "aside"
    "audio" "b" "base" "basefont" "bdo" "big" "blockquote" "body"
    "br" "button" "canvas" "caption" "center" "cite" "code" "col"
    "colgroup" "command" "datalist" "dd" "del" "details" "dialog" "dfn"
    "dir" "div" "dl" "dt" "em" "embed" "fieldset" "figure" "font" "footer"
    "form" "frame" "frameset" "h1" "h2" "h3" "h4" "h5" "h6"
    "head" "header" "hgroup" "hr" "html" "i"
    "iframe" "img" "input" "ins" "keygen" "kbd" "label" "legend" "li" "link"
    "map" "mark" "menu" "meta" "meter" "nav" "noframes" "noscript" "object"
    "ol" "optgroup" "option" "output" "p" "param" "pre" "progress" "q" "rp"
    "rt" "ruby" "s" "samp" "script" "section" "select" "small" "source" "span"
    "strike" "strong" "style" "sub" "sup" "table" "tbody" "td" "textarea" "tfoot"
    "th" "thead" "time" "title" "tr" "tt" "u" "ul" "var" "video" "xmp")
  "A list of all valid HTML4/5 tag names.")

(defvar emblemjs-html5-tags-re
  (concat "^ *\\(" (regexp-opt emblemjs-html5-tags 'words) "\/?\\)")
  "An HTML5 tag with optional leading whitespace")


(defconst emblemjs-font-lock-keywords
  `(
    ;; comment line, block will be handled in emblemjs-extend-region
    (,(emblemjs-nested-re "/.*")
     0 font-lock-comment-face)
    ;; Ember component
    ("^ *\\([a-z]+\\(?:-[a-z]+\\)+\\)"
     1 font-lock-function-name-face)
    ;; html5 tag
    (,emblemjs-tag-re
     1 font-lock-builtin-face)
    ;; arbitrary tag
    ("^ *[a-zA-Z0-9.-]* *\\(%\\([a-z0-9-]+\/?\\)+\\)"
     1 font-lock-builtin-face)    
    ;; #id
    ("^ *[a-zA-Z0-9%. -]*\\(#[a-z0-9_-]+\/?\\)"
     1 font-lock-keyword-face)
    ;; .class
    ("^ *[a-zA-Z0-9%. #_-]*\\(\\(\\.[a-z0-9_-]+\/?\\)+\\)"
     1 font-lock-type-face)
    ;; Ember helper, starts with a capital letter
    ("^ *\\([A-Z]+[.a-zA-Z]*\\)"
     1 font-lock-function-name-face)
    ))


(defconst emblemjs-embedded-re "^ *[a-z0-9_-]+:")
(defconst emblemjs-comment-re  "^ */")

(defun* emblemjs-extend-region ()
  "Extend the font-lock region to encompass embedded engines and comments."
  (let ((old-beg font-lock-beg)
        (old-end font-lock-end))
    (save-excursion
      (goto-char font-lock-beg)
      (beginning-of-line)
      (unless (or (looking-at emblemjs-embedded-re)
                  (looking-at emblemjs-comment-re))
        (return-from emblemjs-extend-region))
      (setq font-lock-beg (point))
      (emblemjs-forward-sexp)
      (beginning-of-line)
      (setq font-lock-end (max font-lock-end (point))))
    (or (/= old-beg font-lock-beg)
        (/= old-end font-lock-end))))


;; Mode setup

(defvar emblemjs-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table in use in emblemjs-mode buffers.")

(defvar emblemjs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [backspace] 'emblemjs-electric-backspace)
    (define-key map "\C-?" 'emblemjs-electric-backspace)
    (define-key map "\C-c\C-f" 'emblemjs-forward-sexp)
    (define-key map "\C-c\C-b" 'emblemjs-backward-sexp)
    (define-key map "\C-c\C-u" 'emblemjs-up-list)
    (define-key map "\C-c\C-d" 'emblemjs-down-list)
    (define-key map "\C-c\C-k" 'emblemjs-kill-line-and-indent)
    map))

;; For compatibility with Emacs < 24, derive conditionally
(defalias 'emblemjs-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode emblemjs-mode emblemjs-parent-mode "Emblemjs"
  "Major mode for editing Emblemjs files.

\\{emblemjs-mode-map}"
  (set-syntax-table emblemjs-mode-syntax-table)
  (add-to-list 'font-lock-extend-region-functions 'emblemjs-extend-region)
  (set (make-local-variable 'font-lock-multiline) t)
  (set (make-local-variable 'indent-line-function) 'emblemjs-indent-line)
  (set (make-local-variable 'indent-region-function) 'emblemjs-indent-region)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (setq comment-start "/")
  (setq indent-tabs-mode nil)
  (setq font-lock-defaults '((emblemjs-font-lock-keywords) nil t)))

;; Useful functions

(defun emblemjs-comment-block ()
  "Comment the current block of Emblemjs code."
  (interactive)
  (save-excursion
    (let ((indent (current-indentation)))
      (back-to-indentation)
      (insert "/")
      (newline)
      (indent-to indent)
      (beginning-of-line)
      (emblemjs-mark-sexp)
      (emblemjs-reindent-region-by emblemjs-indent-offset))))

(defun emblemjs-uncomment-block ()
  "Uncomment the current block of Emblemjs code."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (while (not (looking-at emblemjs-comment-re))
      (emblemjs-up-list)
      (beginning-of-line))
    (emblemjs-mark-sexp)
    (kill-line 1)
    (emblemjs-reindent-region-by (- emblemjs-indent-offset))))

;; Navigation

(defun emblemjs-forward-through-whitespace (&optional backward)
  "Move the point forward at least one line, until it reaches
either the end of the buffer or a line with no whitespace.

If `backward' is non-nil, move the point backward instead."
  (let ((arg (if backward -1 1))
        (endp (if backward 'bobp 'eobp)))
    (loop do (forward-line arg)
          while (and (not (funcall endp))
                     (looking-at "^[ \t]*$")))))

(defun emblemjs-at-indent-p ()
  "Returns whether or not the point is at the first
non-whitespace character in a line or whitespace preceding that
character."
  (let ((opoint (point)))
    (save-excursion
      (back-to-indentation)
      (>= (point) opoint))))

(defun emblemjs-forward-sexp (&optional arg)
  "Move forward across one nested expression.
With `arg', do it that many times.  Negative arg -N means move
backward across N balanced expressions.

A sexp in Emblemjs is defined as a line of Emblemjs code as well as any
lines nested beneath it."
  (interactive "p")
  (or arg (setq arg 1))
  (if (and (< arg 0) (not (emblemjs-at-indent-p)))
      (back-to-indentation)
    (while (/= arg 0)
      (let ((indent (current-indentation)))
        (loop do (emblemjs-forward-through-whitespace (< arg 0))
              while (and (not (eobp))
                         (not (bobp))
                         (> (current-indentation) indent)))
        (back-to-indentation)
        (setq arg (+ arg (if (> arg 0) -1 1)))))))

(defun emblemjs-backward-sexp (&optional arg)
  "Move backward across one nested expression.
With ARG, do it that many times.  Negative arg -N means move
forward across N balanced expressions.

A sexp in Emblemjs is defined as a line of Emblemjs code as well as any
lines nested beneath it."
  (interactive "p")
  (emblemjs-forward-sexp (if arg (- arg) -1)))

(defun emblemjs-up-list (&optional arg)
  "Move out of one level of nesting.
With ARG, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (let ((indent (current-indentation)))
      (loop do (emblemjs-forward-through-whitespace t)
            while (and (not (bobp))
                       (>= (current-indentation) indent)))
      (setq arg (- arg 1))))
  (back-to-indentation))

(defun emblemjs-down-list (&optional arg)
  "Move down one level of nesting.
With ARG, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (let ((indent (current-indentation)))
      (emblemjs-forward-through-whitespace)
      (when (<= (current-indentation) indent)
        (emblemjs-forward-through-whitespace t)
        (back-to-indentation)
        (error "Nothing is nested beneath this line"))
      (setq arg (- arg 1))))
  (back-to-indentation))

(defun emblemjs-mark-sexp ()
  "Marks the next Emblemjs block."
  (let ((forward-sexp-function 'emblemjs-forward-sexp))
    (mark-sexp)))

(defun emblemjs-mark-sexp-but-not-next-line ()
  "Marks the next Emblemjs block, but puts the mark at the end of the
last line of the sexp rather than the first non-whitespace
character of the next line."
  (emblemjs-mark-sexp)
  (let ((pos-of-end-of-line (save-excursion
                              (goto-char (mark))
                              (end-of-line)
                              (point))))
    (when (/= pos-of-end-of-line (mark))
      (set-mark
       (save-excursion
         (goto-char (mark))
         (forward-line -1)
         (end-of-line)
         (point))))))

;; Indentation and electric keys

(defun emblemjs-indent-p ()
  "Returns true if the current line can have lines nested beneath it."
  (loop for opener in emblemjs-block-openers
        if (looking-at opener) return t
        finally return nil))

(defun emblemjs-compute-indentation ()
  "Calculate the maximum sensible indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (if (bobp) 0
      (emblemjs-forward-through-whitespace t)
      (+ (current-indentation)
         (if (funcall emblemjs-indent-function) emblemjs-indent-offset
           0)))))

(defun emblemjs-indent-region (start end)
  "Indent each nonblank line in the region.
This is done by indenting the first line based on
`emblemjs-compute-indentation' and preserving the relative
indentation of the rest of the region.

If this command is used multiple times in a row, it will cycle
between possible indentations."
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (let (this-line-column current-column
          (next-line-column
           (if (and (equal last-command this-command) (/= (current-indentation) 0))
               (* (/ (- (current-indentation) 1) emblemjs-indent-offset) emblemjs-indent-offset)
             (emblemjs-compute-indentation))))
      (while (< (point) end)
        (setq this-line-column next-line-column
              current-column (current-indentation))
        ;; Delete whitespace chars at beginning of line
        (delete-horizontal-space)
        (unless (eolp)
          (setq next-line-column (save-excursion
                                   (loop do (forward-line 1)
                                         while (and (not (eobp)) (looking-at "^[ \t]*$")))
                                   (+ this-line-column
                                      (- (current-indentation) current-column))))
          ;; Don't indent an empty line
          (unless (eolp) (indent-to this-line-column)))
        (forward-line 1)))
    (move-marker end nil)))

(defun emblemjs-indent-line ()
  "Indent the current line.
The first time this command is used, the line will be indented to the
maximum sensible indentation.  Each immediately subsequent usage will
back-dent the line by `emblemjs-indent-offset' spaces.  On reaching column
0, it will cycle back to the maximum sensible indentation."
  (interactive "*")
  (let ((ci (current-indentation))
        (cc (current-column))
        (need (emblemjs-compute-indentation)))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (if (and (equal last-command this-command) (/= ci 0))
          (indent-to (* (/ (- ci 1) emblemjs-indent-offset) emblemjs-indent-offset))
        (indent-to need)))
      (if (< (current-column) (current-indentation))
          (forward-to-indentation 0))))

(defun emblemjs-reindent-region-by (n)
  "Add N spaces to the beginning of each line in the region.
If N is negative, will remove the spaces instead.  Assumes all
lines in the region have indentation >= that of the first line."
  (let ((ci (current-indentation))
        (bound (mark)))
    (save-excursion
      (while (re-search-forward (concat "^" (make-string ci ? )) bound t)
        (replace-match (make-string (max 0 (+ ci n)) ? ) bound nil)))))

(defun emblemjs-electric-backspace (arg)
  "Delete characters or back-dent the current line.
If invoked following only whitespace on a line, will back-dent
the line and all nested lines to the immediately previous
multiple of `emblemjs-indent-offset' spaces.

Set `emblemjs-backspace-backdents-nesting' to nil to just back-dent
the current line."
  (interactive "*p")
  (if (or (/= (current-indentation) (current-column))
          (bolp)
          (looking-at "^[ \t]+$"))
      (backward-delete-char arg)
    (save-excursion
      (let ((ci (current-column)))
        (beginning-of-line)
        (if emblemjs-backspace-backdents-nesting
            (emblemjs-mark-sexp-but-not-next-line)
          (set-mark (save-excursion (end-of-line) (point))))
        (emblemjs-reindent-region-by (* (- arg) emblemjs-indent-offset))
        (back-to-indentation)
        (pop-mark)))))

(defun emblemjs-kill-line-and-indent ()
  "Kill the current line, and re-indent all lines nested beneath it."
  (interactive)
  (beginning-of-line)
  (emblemjs-mark-sexp-but-not-next-line)
  (kill-line 1)
  (emblemjs-reindent-region-by (* -1 emblemjs-indent-offset)))

(defun emblemjs-indent-string ()
  "Return the indentation string for `emblemjs-indent-offset'."
  (mapconcat 'identity (make-list emblemjs-indent-offset " ") ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.emblemjs\\'" . emblemjs-mode))

;; Setup/Activation
(provide 'emblemjs-mode)

;;; emblemjs-mode.el ends here
