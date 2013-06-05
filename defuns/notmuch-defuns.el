(defun notmuch-search-update-tags (&optional pos)
  "Assynchronously update the thread at POS or point from the
  notmuch db."
  (when (eq major-mode 'notmuch-search-mode)
    (lexical-let* ((buf (current-buffer))
                   (pos (or pos (point)))
                   (prev-result (notmuch-search-get-result pos)))
      (let ((threadid (plist-get prev-result :thread)))
        (when threadid
          (deferred:$
            (notmuch-call-notmuch-json-deferred
             "search" "--format=json" "--format-version=1"
             (concat "thread:" threadid))
            (deferred:nextc it
              (lambda (result)
                (setq result (plist-put prev-result :tags
                                        (plist-get (car result) :tags)))
                (with-current-buffer buf
                  (notmuch-search-update-result result pos))))))))))
