(defun init ()
  (set-db-conn)
  (dotimes (_ (cconf 'poolsize))
    (dbi:connect-cached :mysql
                        :database-name (car conn-str)
                        :username (nth 1 conn-str)
                        :password (nth 2 conn-str)))
  (ping) ;; test db conn  
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
  (setf (hunchentoot:content-type*) "application/json")
  (let ((separator (if (< 1 (get-version version)) "," "||")))
    (multiple-value-bind (result msg) (post-valid-p post_nr regions board separator)
      (cond
        (result
         (insert-post post_nr board msg)
         (format nil "{\"~a\": [~{\"~a\"~^,~}]}~%" post_nr msg))
        (t
         (format nil "{\"Error\": \"~a\"}~%" msg))))))

(handle :post (api-get :uri "/api/get")
    (post_nrs board version)
  (@json tbnl:*reply*)
  (setf post_nrs (str:split "," post_nrs))
  (cond
    ((and (loop for x in post_nrs always (post-number-p x))
          (boardp board))
     (format nil "~a~%" (get-posts post_nrs board)))
    (t (format nil "~a~%" "bad"))))

(handle :get (api-flags :uri "/api/flags")
    ()
  (@plain tbnl:*reply*)
  (format nil "~a~%" *flags-txt*))
