(defvar chat-track-last-buffer nil
  "Last non-chat buffer")

(defun chat-track-switch-buffer ()
  "Return the buffer name of ARG in `erc-modified-channels-alist'.
Negative arguments index in the opposite direction.  This direction is
relative to `erc-track-switch-direction'"
  (interactive)
  (unless (memq major-mode '(jabber-chat-mode erc-mode))
    (setq chat-track-last-buffer (current-buffer)))
  (cond
   ((and (boundp 'jabber-activity-jids)
         jabber-activity-jids)
    (jabber-activity-switch-to))
   ((and (boundp 'erc-modified-channels-alist)
         erc-modified-channels-alist)
    (erc-track-switch-buffer 0))
   (t
    (switch-to-buffer chat-track-last-buffer))))

(eval-after-load 'erc-track
  '(setq erc-track-enable-keybindings nil))
(global-set-key (kbd "C-c C-SPC") 'chat-track-switch-buffer)

(provide 'setup-chat-tracking)
