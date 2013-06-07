;;; newline-mark in whitespace-style leads to strange column jumping
;;; after empty lines and interferes with popup.el, so remove it from
;;; the default setting

;; (cl-callf2 delq 'newline-mark whitespace-style)

;; shows
;; lines-tail  - lines longer than 80 chars
;; trailing    - trailing whitespace
;; empty       - empty lines at the beginning and end of the buffer
;; indentation - evil leading indentation marks
;;               (spaces w/ tab-indent-mode, tabs w/o)
(setq whitespace-style '(face lines-tail trailing empty indentation))

(provide 'setup-whitespace)
