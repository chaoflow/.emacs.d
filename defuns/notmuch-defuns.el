(defun notmuch-search-update-tags (&optional pos)
  (when (eq major-mode 'notmuch-search-mode)
    (setq pos (or pos (point)))
    (let* ((old-result (notmuch-search-get-result pos))
           (threadid (plist-get old-result :thread)))
      (when threadid
        (notmuch-search-update-result
         (plist-put old-result :tags
                    (plist-get (car
                                (notmuch-call-notmuch-json
                                 "search" "--format=json" "--format-version=1"
                                 "--exclude=false"
                                 (concat "thread:" threadid)))
                               :tags))
         pos)))))
