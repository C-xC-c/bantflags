;; (C) Copyright 2020 C-xC-c <boku@plum.moe>
;; This file is part of bantflags.
;; bantflags is licensed under the GNU AGPL Version 3.0 or later.
;; see the LICENSE file or <https://www.gnu.org/licenses/>
(in-package :bantflags)

(defvar *serb* nil
  "The hunchentoot acceptor that serves requests")

(defun configure ()
  (setf conn (config-item :db-conn))
  ;; We need to write an actual connection pool here
  (loop repeat (- (config-item :poolsize) (length (clsql:connected-databases)))
        do (clsql:connect conn :database-type :mysql :pool t :if-exists :new))
  (set-boards)
  (set-flags)
  (setf *serb* (make-instance 'toot:acceptor
                              :port (config-item :port)
                              :name 'bantflags-acceptor)))

(defun start ()
  (configure)
  (when (null *serb*)
    (error "serb is nil? ehh??"))
  (hunchentoot:start *serb*))

(defun tear-down (&aux (dbs (clsql:connected-databases)))
  (when dbs (loop for db in dbs do (clsql:disconnect :database db))))

(defun stop ()
  (tear-down)
  (when (hunchentoot:started-p *serb*)
    (hunchentoot:stop *serb* :soft t)))

(toot:handle (api-post :post "/api/post"
                       :acceptor 'bantflags-acceptor
                       :content-type toot:@json)
    (post_nr regions board version)
  (multiple-value-bind (result msg) (insert-post-p post_nr (cl-ppcre:split "," regions) board)
    (cond
      (result
       (insert-post post_nr board msg)
       (format nil "{\"~a\": [~{\"~a\"~^,~}]}~%" post_nr msg)) ;; This makes JSON
      (t (format nil "{\"Error\": \"~a\"}~%" msg)))))

(toot:handle (api-get :post "/api/get"
                      :acceptor 'bantflags-acceptor
                      :content-type toot:@json)
    (post_nrs board version)
  (setf post_nrs (cl-ppcre:split "," post_nrs))
  (if (get-posts-p post_nrs board)
      (format nil "~a" (get-posts post_nrs board))
      "{[\"bad\"]}"))

(toot:handle (api-flags :get "/api/flags"
                        :acceptor 'bantflags-acceptor
                        :content-type toot:@plain)
    ()
  *flags-txt*)
