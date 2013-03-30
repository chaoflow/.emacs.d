;;; Load AucTeX
(load "auctex.el" nil t t)

;;; Load Preview LaTeX
(load "preview-latex.el" nil t t)

;;; RefTeX
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode

;;; Prefer pdfLaTeX
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

(provide 'setup-latex)
