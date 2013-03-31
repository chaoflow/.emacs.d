;;; org

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c l") 'org-store-link)

(define-key org-mode-map (kbd "C-c f") 'org-footnote-action)

(setq org-log-into-drawer t
      org-todo-keywords '((sequence "❢" "✔"))
      org-footnote-auto-adjust t
      
      ;; org-goto-interface 'outline-path-completion
      )

;;; encryption using org-crypt
(require 'org-crypt)
(org-crypt-use-before-save-magic)
;; (setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key "704C9C91")


;;; org export
(require 'ox-latex)
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

(add-to-list 'org-latex-classes
             '("koma-article"
               "\\documentclass{scrartcl}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq org-latex-pdf-process '("rubber -d --into %o %f")
      org-latex-remove-logfiles nil)

(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

(require 'org-special-blocks)

;;; paper stuff
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
; (add-hook 'org-mode-hook 'turn-on-reftex)
(setq reftex-default-bibliography '("/home/coroa/papers/refs.bib"))

(defun org-mode-reftex-search ()
  ;;jump to the notes for the paper pointed to at from reftex search
  (interactive)
  (let ((citation (reftex-citation t)))
    (org-open-link-from-string
     (format "[[notes:%s]]" (if (listp citation) (car citation) citation)))))
  
(defun org-mode-reftex-setup ()
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
  ; (reftex-parse-all)
       (reftex-set-cite-format           ;add a custom reftex cite format to insert links
        '((?b . "[[bib:%l][%l-bib]]")
          (?n . "[[notes:%l][%l-notes]]")
          (?c . "[[cite:%l]]")
          (?p . "[[papers:%l][%l-pdf]]")
          (?t . "%t")
          (?h . "%t\n:PROPERTIES:\n:Custom_ID: %l\n:END:\n[[papers:%l][%l-pdf]]\n%z"))))
    
  (local-set-key (kbd "C-c )") 'reftex-citation)
  (local-set-key (kbd "C-c (") 'org-mode-reftex-search))
  
(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(setq org-link-abbrev-alist
      '(("bib" . "~/papers/refs.bib::%s")
        ("notes" . "~/papers/papers.org::#%s")
        ("papers" . "~/papers/%s.pdf")
        ("xoj" . "~/papers/%s.xoj")))

(org-add-link-type 
 "cite"
 (lambda (path) (org-open-file "~/papers/papers.org" nil nil (concat "#" path)))
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (cl-search "cite:" desc)))
         (format "\\cite{%s}" path)
       (format "\\cite[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))

;;; maps

(add-to-list 'org-link-abbrev-alist
             '("omap"
               . "http://nominatim.openstreetmap.org/search?q=%s&polygon=1"))
(add-to-list 'org-link-abbrev-alist
             '("gmap"
               . "http://maps.google.com/maps?q=%s"))

;;; appointment warning

(require 'org-agenda)
(require 'appt)

(defun jho/update-appt-from-org ()
  ; (setq appt-time-msg-list nil)
  (org-agenda-to-appt 'refresh))

;; When use 'r' (rebuild agenda) reload appt
(add-hook 'org-agenda-mode-hook 'jho/update-appt-from-org)

;; update appt each time agenda opened
(add-hook 'org-finalize-agenda-hook 'jho/update-appt-from-org)

;; turn appointment checking on
(jho/update-appt-from-org)
(appt-activate 1)

;; time in minutes before an appointment that the warning begins
(setq appt-message-warning-time 15)  ; 12
;; number of minutes to wait between checking the appointment list
(setq appt-display-interval 5)  ; 3


;;; german calendar
(add-hook 'calendar-load-hook
          (lambda ()
            (calendar-set-date-style 'european)))

(setq calendar-week-start-day 1
      calendar-day-name-array ["Sonntag" "Montag" "Dienstag" "Mittwoch"
                               "Donnerstag" "Freitag" "Samstag"]
      calendar-month-name-array ["Januar" "Februar" "März" "April" "Mai"
                                 "Juni" "Juli" "August" "September"
                                 "Oktober" "November" "Dezember"])

(setq calendar-christian-all-holidays-flag nil
      solar-n-hemi-seasons
      '("Frühlingsanfang" "Sommeranfang" "Herbstanfang" "Winteranfang")
      holiday-bahai-holidays nil
      holiday-general-holidays
      '((holiday-fixed 1 1 "Neujahr")
        (holiday-fixed 5 1 "1. Mai")
        (holiday-fixed 10 3 "Tag der Deutschen Einheit"))
      holiday-christian-holidays
      '((holiday-float 12 0 -4 "1. Advent" 24)
        (holiday-float 12 0 -3 "2. Advent" 24)
        (holiday-float 12 0 -2 "3. Advent" 24)
        (holiday-float 12 0 -1 "4. Advent" 24)
        (holiday-fixed 12 25 "1. Weihnachtstag")
        (holiday-fixed 12 26 "2. Weihnachtstag")
        (holiday-fixed 1 6 "Heilige Drei Könige")
        (holiday-easter-etc -48 "Rosenmontag (BY)")
        ;; (holiday-easter-etc -3 "Gründonnerstag")
        (holiday-easter-etc  -2 "Karfreitag")
        (holiday-easter-etc   0 "Ostersonntag")
        (holiday-easter-etc  +1 "Ostermontag")
        (holiday-easter-etc +39 "Christi Himmelfahrt")
        (holiday-easter-etc +49 "Pfingstsonntag")
        (holiday-easter-etc +50 "Pfingstmontag")
        (holiday-easter-etc +60 "Fronleichnam")
        (holiday-fixed 8 15 "Mariae Himmelfahrt")
        (holiday-fixed 11 1 "Allerheiligen")
        ;; (holiday-float 11 3 1 "Buss- und Bettag" 16)
        (holiday-float 11 0 1 "Totensonntag" 20))
      holiday-islamic-holidays nil
      holiday-hebrew-holidays nil
      holiday-oriental-holidays nil
      holiday-other-holidays nil
      ;; '((holiday-fixed 1 1 "New Year's Day")
      ;;   (holiday-fixed 5 1 "Labour Day")
      ;;   (holiday-fixed 12 25 "Christmas Day")
      ;;   (holiday-fixed 12 26 "St Stephen's Day")
      ;;   (holiday-fixed 10 3 "German Unity Day")
      ;;   (holiday-fixed 10 31 "Reformation Day")
      ;;   (holiday-float 11 3 -1 "Day of Repentance and Prayer" 22)
      ;;   (holiday-easter-etc -2 "Good Friday")
      ;;   (holiday-easter-etc 0 "Easter Day")
      ;;   (holiday-easter-etc 1 "Easter Monday")
      ;;   (holiday-easter-etc 39 "Ascension Day")
      ;;   (holiday-easter-etc 49 "Pentecost (Whitsunday)")
      ;;   (holiday-easter-etc 50 "Whit Monday"))
      )

(setq calendar-latitude 48.2085
      calendar-longitude 16.3731)


;; babel

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (R . t)
   (perl . t)
   (ruby . t)
   (python . t)
   (js . t)
   (haskell . t)
   (clojure . t)
   (ditaa . t)
   (latex . t)))

(setq org-src-fontify-natively t
      org-src-preserve-indentation t
      org-src-tab-acts-natively t)


;;; org inline tasks
(require 'org-inlinetask)

;; (defun jho/org-latex-format-inlinetask (todo type priority name tags contents)
;;   "Format an inline task element for LaTeX export."
;;   (when (eq type 'todo)
;;     (let ((full-title
;;            (concat
;;             (when todo
;;               (format "\\textbf{\\textsf{\\textsc{%s}}} " todo))
;;             (when priority (format "\\framebox{\\#%c} " priority))
;;             title)))
;;       (format "\\XXX{%s}\n" full-title))))

;; (setq org-latex-format-inlinetask-function 'jho/org-latex-format-inlinetask)

(provide 'user-org)
