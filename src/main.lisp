(defun init ()
  (set-db-conn)
  (dotimes (_ (cconf 'poolsize))
    (clsql:connect conn :database-type :mysql :pool t :if-exists :new))
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

(handle :post (api-post :uri "/api/post")
    (post_nr regions board version)
  (@json tbnl:*reply*)
  (setf regions (cl-ppcre:split "," regions))
  (multiple-value-bind (result msg) (post-valid-p post_nr regions board)
    (cond
      (result
       (insert-post post_nr board msg)
       (format nil "{\"~a\": [~{\"~a\"~^,~}]}~%" post_nr msg))
      (t (format nil "{\"Error\": \"~a\"}~%" msg)))))

(handle :post (api-get :uri "/api/get")
    (post_nrs board version)
  (@json tbnl:*reply*)
  (setf post_nrs (cl-ppcre:split "," post_nrs))
  (cond
    ((and (not (null post_nrs))
          (every #'post-number-p post_nrs)
          (boardp board))
     (format nil "~a~%" (get-posts post_nrs board)))
    (t (format nil "{[\"~a\"]}~%" "bad"))))

(handle :get (api-flags :uri "/api/flags")
    ()
  (@plain tbnl:*reply*)
  (format nil "~a~%" *flags-txt*))
