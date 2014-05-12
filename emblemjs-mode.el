;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.emblem\\'")  'emblemjs-mode))

(defgroup emblemjs nil
  "Emblem.js template support for Emacs"
  :group 'languages
  :prefix "emblemjs-")

;; copied from slim-mode
(defconst emblemjs-html5-tags
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
  "A list of all valid HTML5 tag names.")

(defconst emblemjs-tag-re
  (concat "^ *\\(" (regexp-opt emblemjs-html5-tags 'words) "\/?\\)")
  "An HTML5 tag with optional leading whitespace")


;; Helper for nested block (comment, embedded, text) - slim
(defun emblemjs-nested-re (re)
  (concat "^\\( *\\)" re "\n\\(?:\\(?:\\1 .*\\)\n\\)+"))


(defconst emblemjs-font-lock-keywords
  `(;; comment block
    ("^\\( *\\)/.*\n\\(?:\\(?:\\1 .*\\)\n\\)+"
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


;;; Indentation

(defcustom emblemjs-indent-offset 2
  "Default indentation offset for Emblem.js"
  :type 'integer
  :group 'emblemjs)

(defcustom emblemjs-indent-trigger-commands
  '(indent-for-tab-command)
  "Commands that might trigger a `emblemjs-indent-line' call."
  :type '(repeat symbol)
  :group 'emblemjs)

(defvar emblemjs-indent-current-level 0
  "Current indentation level `emblemjs-indent-line-function' is using.")

(defvar emblemjs-indent-levels '(0)
  "Levels of indentation available for `emblemjs-indent-line-function'.")

(defvar emblemjs-indent-dedenters '("else")
  "List of words that should be dedented.
These make `emblemjs-indent-calculate-indentation' subtract the value of
`emblemjs-indent-offset'.")


(defconst emblemjs-comment-re  "^ */.*")

(defun* emblemjs-extend-region ()
  "Extend the font-lock region to encompass embedded engines and comments."
  (let ((old-beg font-lock-beg)
        (old-end font-lock-end))
    (save-excursion
      (goto-char font-lock-beg)
      (beginning-of-line)
      (unless (looking-at emblemjs-comment-re)
        (return-from emblemjs-extend-region))
      (setq font-lock-beg (point))
      (emblemjs-forward-sexp)
      (beginning-of-line)
      (setq font-lock-end (max font-lock-end (point))))
    (or (/= old-beg font-lock-beg)
        (/= old-end font-lock-end))))


(define-derived-mode emblemjs-mode fundamental-mode "Emblemjs"
  "Major mode for editing Emblem.js files"
;  (add-to-list 'font-lock-extend-region-functions 'emblemjs-extend-region)
  (setq font-lock-defaults '((emblemjs-font-lock-keywords) nil nil)))

(provide 'emblemjs-mode)
