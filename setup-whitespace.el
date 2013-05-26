;;; newline-mark in whitespace-style leads to strange column jumping
;;; after empty lines and interferes with popup.el, so remove it from
;;; the default setting

(cl-callf2 delq 'whitespace-mark whitespace-style)

(provide 'setup-whitespace)
