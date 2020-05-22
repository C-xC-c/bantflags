(defun init ()
  (set-db-conn)
  (dotimes (_ (cconf 'poolsize))
    (clsql:connect conn :database-type :mysql :pool t :if-exists :new))
  (when (eq nil clsql:*default-database*)
    (error "fucked up connecting to database"))
  (set-boards)
  (set-flags)
  (defvar +serb+ (make-instance 'hunchentoot:easy-acceptor
                                :port 4242
                                :document-root (cconf 'www-root)))
  (hunchentoot:start +serb+))

(defun main ()
  (handler-case (init)
    (error (c)
      (format t "Init fucked up, exiting ~a" c)
      (return-from main)))
  
  (loop (sleep 43200) (gc :full t)))

(defmethod hunchentoot:acceptor-status-message (acceptor (http-status-code (eql 404)) &key)
  (format nil ""))

(defmacro handle (method uri params &body body)
  `(hunchentoot:define-easy-handler ,uri ,params
     (unless (eq ,method (hunchentoot:request-method*))
       (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
       (hunchentoot:abort-request-handler))
     ,@body))

(defun host-dir (uri path)
  (push
   (hunchentoot:create-folder-dispatcher-and-handler uri path)
   hunchentoot:*dispatch-table*))

(host-dir "/flags/" (merge-pathnames #p"flags/" (cconf 'www-root)))

(handle :post (api-post :uri "/post")
    (post_nr regions board verison)
  (@json)
  (let ((separator (if (< 1 (get-version version)) "," "||")))
    (multiple-value-bind (result msg) (post-valid-p post_nr regions board separator)
      (cond
        (result
         (insert-post post_nr board msg)
         (format nil "{\"~a\": [~{\"~a\"~^,~}]}~%" post_nr msg))
        (t
         (format nil "{\"Error\": \"~a\"}~%" msg))))))

(handle :get (api-flags :uri "/flags")
    ()
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "~a~%" *flags-txt*))

(handle :post (api-get :uri "/get")
    (post_nrs board version)
  (setf post_nrs (str:split "," post_nrs))
  (cond
    ((and (loop for x in post_nrs always (post-number-p x))
          (boardp board))
     (format nil "~a" (get-posts post_nrs board)))
    (t (format nil "~a~%" "bad"))))
