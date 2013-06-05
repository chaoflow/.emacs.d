(when (require 'deferred nil t)

  (defmacro deferred:$$ (&rest elements)
    "Anaphoric function chain macro for deferred/cancelable chains."
    (declare (debug (&rest form)))
    `(let (it objects)
       ,@(loop for i in elements
               collect
               `(push (setq it ,i) objects))
       (setf (deferred-cancel it)
             #'(lambda (x)
                 (loop for o in objects
                       do (deferred:cancel o))))
       it))

  (defun deferred:process-shell-process-buffer (command &rest args)
    (lexical-let (proc)
      (deferred:$$
        (deferred:process-buffer-gen
          #'(lambda (name buffer command &rest args)
              (setq proc (apply 'start-process-shell-command name buffer command args)))
          command args)
        (deferred:nextc it
          #'(lambda (buffer)
              (cons proc buffer))))))

  (defun notmuch-call-notmuch-json-deferred (&rest args)
    "Invoke `notmuch-command' with `args' and return the parsed JSON output.

  The returned output will represent objects using property lists
  and arrays as lists.  If notmuch exits with a non-zero status,
  this will pop up a buffer containing notmuch's output and
  signal an error."

    (lexical-let
        ((args args)
         (err-file (make-temp-file "nmerr")))
      (deferred:$$
        (deferred:process-shell-process-buffer
          (format "%s 2> %s"
                  (combine-and-quote-strings (cons notmuch-command args))
                  err-file))
        (deferred:nextc it
          (lambda (x)
            (let ((proc (car x))
                  (buf  (cdr x)))
              (prog1
                  (with-current-buffer buf
                    (notmuch-check-exit-status
                     (process-exit-status proc)
                     (cons notmuch-command args)
                     (buffer-string)
                     err-file)
                    (goto-char (point-min))
                    (let ((json-object-type 'plist)
                          (json-array-type 'list)
                          (json-false 'nil))
                      (json-read)))
                (kill-buffer buf)))))
        (deferred:watch it
          (lambda (x)
            (delete-file err-file)))))))

