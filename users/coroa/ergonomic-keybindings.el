;-*- coding: utf-8 -*-
;; ergonomic_keybinding_qwerty.el -- A ergonomic keybinding for qwerty keyboard.

;; Copyright © 2007 Xah Lee

;; Author: Xah Lee (http://xahlee.org/)
;; Keywords: qwerty, dvorak, keybinding, ergonomic

;; You can redistribute this program and/or modify it under the terms
;; of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later
;; version.

  ;;; Commentary:

;; This keybinding puts the most frequently used emacs shortcuts into
;; the most easier to type spots, and, using the Meta-«key» space only.

;; For detail on the design of this keybinding and a visual layout,
;; and versions for Dvorak or Qwerty keyboards, see:
;; http://xahlee.org/emacs/ergonomic_emacs_keybinding.html

;;; History:
;; version 1.1, 2007-12-18. changed keycode to consistantly use kbd syntax. Fixed a scroll-up and scroll-down mixup.
;; version 1.0, 2007-08-01. first version.

;;; Install:

;; Place this file in your favoritate directory, such as
;; ~/emacs/ergonomic_keybinding_qwerty.el
;; Then, place the following code in your emacs init file (~/.emacs):
;; (load "~/emacs/ergonomic_keybinding_qwerty.el")

;;; Code:
;;; CURSOR MOVEMENTS

;; Single char cursor movement
(global-set-key (kbd "M-n") 'backward-char) ; was none
(global-set-key (kbd "M-i") 'forward-char)  ; was tab-to-tab-stop
(global-set-key (kbd "M-u") 'previous-line) ; was upcase-word
(global-set-key (kbd "M-e") 'next-line) ; was forward-sentence

;; Move by word
(global-set-key (kbd "M-l") 'backward-word) ; was downcase-word
(global-set-key (kbd "M-y") 'forward-word) ; was yank-pop

;; Move by paragraph
(global-set-key (kbd "M-L") 'backward-paragraph) ; was none
(global-set-key (kbd "M-Y") 'forward-paragraph) ; was none

;; ;; Move to beginning/ending of line
(global-set-key (kbd "M-N") 'move-beginning-of-line) ; was none
;;(global-set-key (kbd "M-J") 'backward-to-indentation-this-line);
(global-set-key (kbd "M-I") 'move-end-of-line) ; was none

;; Move by screen (page up/down)
(global-set-key (kbd "M-U") 'scroll-down)  ; was none
(global-set-key (kbd "M-E") 'scroll-up)  ; was none

;; ;; Move to beginning/ending of file
(global-set-key (kbd "M-h") 'forward-sentence) ; was mark-paragraph
(global-set-key (kbd "M-H") 'backward-sentence) ; was none

;; Switch window (emacs's “frame”)
(global-set-key (kbd "M-o") 'other-window) ; was none
(global-set-key (kbd "M-O") 'other-frame) ; was none

(global-set-key (kbd "M-'") 'recenter)  ; was abbrev-prefix-mark


;;; MAJOR EDITING COMMANDS
;(global-set-key (kbd "C-<backspace>") 'backward-delete-char-untabify)
;; was set-mark-command

;; Delete previous word.
(global-set-key (kbd "M-s") 'delete-char) ; was paredit-splice-sexp

; Delete previous word.
(global-set-key (kbd "M-S") 'kill-word) ; was paredit-split-sexp

; Copy Cut Paste, Paste previous
;(global-set-key (kbd "M-w") 'kill-region)     ; was kill-ring-save
(global-set-key (kbd "M-f") 'kill-region)  ; was forward-word
(global-set-key (kbd "M-F") 'kill-rectangle) ; was none
(global-set-key (kbd "M-p") 'yank)            ; was none
(global-set-key (kbd "M-P") 'yank-pop)  ; was none
; browse kill ring
;(global-set-key (kbd "M-Y") 'browse-kill-ring)


; Undo
;(global-set-key (kbd "M-d") 'undo) ; was kill-word

; Kill line
;; (global-set-key (kbd "M-t") 'kill-line) ; was transpose-words
;(global-unset-key (kbd "C-k")) ; was kill-line; prevent unintentional deletion of line

;; (defun copy-line (arg)
;;   "Copy lines (as many as prefix argument) in the kill ring"
;;   (interactive "p")
;;   (kill-ring-save (line-beginning-position)
;;                   (line-beginning-position (+ 1 arg)))
;;   (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

;; (global-set-key (kbd "M-T") 'copy-line) ; was none

;;; Textual Transformation



;;; EMACS'S SPECIAL COMMANDS

; Mark point. 32 is the ascii code for space
(global-set-key (kbd "M-SPC") 'set-mark-command) ; was just-one-space
(global-set-key (kbd "M-S-SPC") 'mark-paragraph) ; was none

;(global-set-key (kbd "M-a") 'execute-extended-command) ; was backward-sentence

;;; WINDOW SPLITING
(global-set-key (kbd "M-3") 'split-window-horizontally); was digit argument
(global-set-key (kbd "M-2") 'split-window-vertically) ; was digit-argument
(global-set-key (kbd "M-1") 'delete-other-windows) ; was digit-argument
(global-set-key (kbd "M-0") 'delete-window) ; was digit-argument
;(global-set-key (kbd "M-s") 'other-window) ; was center-line


; line scrolling
(defun scroll-down-one-line ()
  "Scroll down one line."
  (interactive)
  (scroll-down 1))

(defun scroll-up-one-line ()
  "Scroll up one line."
  (interactive)
  (scroll-up 1))

(global-set-key (kbd "M-.") 'scroll-up-one-line)
(global-set-key (kbd "M-,") 'scroll-down-one-line)


;;; Org-mode
(defun org-mode-fix-kbd ()
  (local-set-key (kbd "M-e") 'next-line) ; was org-forward-sentence

  (local-set-key (kbd "M-h") 'org-forward-sentence)
  (local-set-key (kbd "M-H") 'org-backward-sentence)

  (local-set-key (kbd "M-n") 'backward-char) ; was org-shiftdown
  (local-set-key (kbd "M-N") 'org-beginning-of-line) ; was none
  (local-set-key (kbd "M-I") 'org-end-of-line) ; was org-toggle-iimage-in-org

  (local-set-key (kbd "M-p") 'org-yank)            ; was none

  (local-set-key (kbd "M-W") 'org-shiftup)
  (local-set-key (kbd "M-R") 'org-shiftdown)
  (local-set-key (kbd "M-A") 'org-shiftleft)
  (local-set-key (kbd "M-S") 'org-shiftright)
  )
(add-hook 'org-mode-hook 'org-mode-fix-kbd)


;;; Message-Mode
(defun message-mode-fix-kbd ()
  (local-set-key (kbd "M-n") 'backward-char)) ; was message-display-abbrev
(add-hook 'message-mode-hook 'message-mode-fix-kbd)

;;; C-mode
(defun c-mode-fix-kbd ()
  (local-set-key (kbd "M-e") 'next-line)) ; was c-end-of-statement
(add-hook 'c-mode-hook 'c-mode-fix-kbd)

(eval-after-load 'elpy
  '(progn
     (define-key elpy-mode-map (kbd "M-e") nil)
     (define-key elpy-mode-map (kbd "M-n") nil)
     (define-key elpy-mode-map (kbd "M-p") nil)
     (define-key elpy-mode-map (kbd "M-h") 'elpy-nav-forward-statement)
     (define-key elpy-mode-map (kbd "M-H") 'elpy-nav-backward-statement)
     (define-key elpy-mode-map (kbd "M-Y") 'elpy-forward-definition)
     (define-key elpy-mode-map (kbd "M-L") 'elpy-backward-definition)))
(provide 'ergonomic-keybindings)
