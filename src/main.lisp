;; (C) Copyright 2020 C-xC-c <boku@plum.moe>
;; This file is part of bantflags.
;; bantflags is licensed under the GNU AGPL Version 3.0 or later.
;; see the LICENSE file or <https://www.gnu.org/licenses/>
(in-package :bantflags)

(defun init ()
  (setf conn (conf 'db-conn))
  (loop repeat (cconf 'poolsize) do
    (clsql:connect conn :database-type :mysql :pool t :if-exists :new))
  (set-boards)
  (set-flags)
  (defvar +serb+ (make-instance 'hunchentoot:easy-acceptor
                                :port (cconf 'port)
                                :document-root (cconf 'www-root)
                                :access-log-destination (cconf 'access-log)
                                :message-log-destination (cconf 'error-log))))

(defun main ()
  (handler-case (init)
    (error (c)
      (format t "Init fucked up, exiting ~a" c)
      (return-from main)))
  (handler-case (hunchentoot:start +serb+)
    (error (c)
      (format t "couldn't start serb: ~a" c)
      (return-from main)))
  (loop (sleep 43200) (gc :full t)))

(defmethod hunchentoot:acceptor-status-message (acceptor (http-status-code (eql 404)) &key)
  (format nil "")) ;; Empty 404 page

(henh:handle :post (api-post :uri "/api/post") @json
    (post_nr regions board version)
  (multiple-value-bind (result msg) (insert-post-p post_nr (cl-ppcre:split "," regions) board)
    (cond
      (result
       (insert-post post_nr board msg)
       (format nil "{\"~a\": [~{\"~a\"~^,~}]}~%" post_nr msg)) ;; This makes JSON
      (t (format nil "{\"Error\": \"~a\"}~%" msg)))))

(henh:handle :post (api-get :uri "/api/get") @json
    (post_nrs board version)
  (if (get-posts-p (cl-ppcre:split "," post_nrs) board)
      (format nil "~a~%" (get-posts post_nrs board))
      (t (format nil "{[\"~a\"]}~%" "bad"))))

(henh:handle :get (api-flags :uri "/api/flags") @plain
    ()
  (format nil "~a~%" *flags-txt*))
