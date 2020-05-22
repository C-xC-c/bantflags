;; Comparing strings with both
(defparameter *flags* (make-hash-table :test 'equal))
(defparameter *boards* (make-hash-table :test 'equal))
(defparameter *flags-txt* nil)
(defparameter conn nil)

(defvar get-posts-sql "SELECT posts.post_nr, flags.flag from flags left join postflags on (postflags.flag = flags.id) left join posts on (postflags.post_nr = posts.id) where posts.post_nr in (~{'~a'~^,~}) and posts.board = '~a';")

(clsql:file-enable-sql-reader-syntax)

(defmacro dbfun (name &rest body)
  `(defun ,name ,(car body)
     (clsql:with-database (db conn :database-type :mysql :pool t)
       ,@(cdr body))))

(defun flag-id (flag)
  (gethash flag *flags*))

(dbfun insert-post (post_nr board flags)
       (clsql:query (format nil "insert ignore into posts (post_nr, board) values (~a, '~a');" post_nr board))
       (let ((post-id (caar (clsql:query (format nil "select id from posts where post_nr = ~a and board = '~a';" post_nr board)))))
         (clsql:with-transaction ()
           (clsql:execute-command
            (with-output-to-string (s)
              (format s "insert into postflags (post_nr, flag) values")
              (loop for flag in (butlast flags)
                    do (format s "(~a,~a)," post-id (flag-id flag)))
              (format s "(~a,~a);" post-id (flag-id (car (last flags))))
              :database db)))))

(dbfun get-posts (posts board)
       (let ((result (clsql:query (format nil get-posts-sql posts board :database db)))
             (table (make-hash-table)))
         (dolist (row result)
           (let ((key (car row)))
             (unless (gethash key table)
               (setf (gethash key table) '())
               (push (cadr row) (gethash key table)))))
         (jojo:to-json table)))

(dbfun get-flags ()
       (clsql:select [id] [flag]
                     :from [flags]
                     :database db))
