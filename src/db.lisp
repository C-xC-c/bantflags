;; Databases in common lisp are the fucking worst.
;; Don't even bother.

;; Comparing strings with both
(defparameter *flags* (make-hash-table :test 'equal))
(defparameter *boards* (make-hash-table :test 'equal))
(defparameter *flags-txt* nil)
(defparameter conn-str nil)

(defvar get-posts-sql "SELECT posts.post_nr, flags.flag from flags left join postflags on (postflags.flag = flags.id) left join posts on (postflags.post_nr = posts.id) where posts.post_nr in (~{'~a'~^,~}) and posts.board = '~a';")

(defun fuck-you-fukamachi (conn |;_;|)
  "What the fuck is going on with what dbi:fetch returns? why is it a
fucking list with the database columns as a symbols with pipes around
them? How in the dicking shit am I supposed to use :|post_nr| 1234 in
any useful or practical way? Why does this fucking Wumpus of a human
being Fukamachi feel the need to duplicate so much data? Don't get me
wrong, clsql is easily worse to work with, but at least it was fucking
smart enough to make the database fields into (values rows columns)"
  (mapcar (lambda (x) (list (nth 1 x) (nth 3 x)))
          (dbi:fetch-all (dbi:execute (dbi:prepare conn |;_;|)))))

(defmacro dbfun (name &rest body)
  `(defun ,name ,(car body)
     (dbi:with-connection  (conn :mysql
                                 :database-name (car conn-str)
                                 :username (nth 1 conn-str)
                                 :password (nth 2 conn-str))
       ,@(cdr body))))

(defun flag-id (flag)
  (gethash flag *flags*))

(dbfun ping ()
       (dbi:ping conn))

(dbfun insert-post (post_nr board flags)
       (dbi:do-sql conn
         (format nil "insert ignore into posts (post_nr, board) values (~a, '~a');" post_nr board))
       (let ((post-id (cadr (dbi:fetch (dbi:execute (dbi:prepare conn "select id from posts where post_nr = 9999 and board = 'bant';"))))))
         (dbi:do-sql conn
             (with-output-to-string (s)
               (format s "insert into postflags (post_nr, flag) values")
               (loop for flag in (butlast flags)
                     do (format s "(~a,~a)," post-id (flag-id flag)))
               (format s "(~a,~a);" post-id (flag-id (car (last flags))))))))

(dbfun get-posts (posts board)
       (let ((result (fuck-you-fukamachi conn (format nil get-posts-sql posts board)))
             (table (make-hash-table)))
         (loop for (post_nr . flag) in result do
           (unless (gethash post_nr table)
             (setf (gethash post_nr table) '()))
           (push (car flag) (gethash post_nr table)))
         (jojo:to-json table)))

(dbfun get-flags ()
       (fuck-you-fukamachi conn "select flags.id, flags.flag from flags"))
