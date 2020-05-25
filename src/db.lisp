;; (C) Copyright 2020 C-xC-c <boku@plum.moe>
;; This file is part of bantflags.
;; bantflags is licensed under the GNU AGPL Version 3.0 or later.
;; see the LICENSE file or <https://www.gnu.org/licenses/>
(in-package #:bantflags)

;; Databases in common lisp are the fucking worst.
;; Don't even bother.

;; We're comparing strings
(defparameter *flags* (make-hash-table :test 'equal))
(defparameter *boards* (make-hash-table :test 'equal))
(defparameter *flags-txt* nil)
(defparameter conn nil)

(defvar get-posts-sql "SELECT posts.post_nr, flags.flag from flags left join postflags on (postflags.flag = flags.id) left join posts on (postflags.post_nr = posts.id) where posts.post_nr in (~{'~a'~^,~}) and posts.board = '~a';")

(defmacro dbfun (name &rest body)
  `(defun ,name ,(car body)
     (clsql:with-database (db conn :database-type :mysql :pool t)
       ,@(cdr body))))

(defun flag-id (flag)
  (gethash flag *flags*))

(dbfun insert-post (post_nr board flags)
       (clsql:execute-command (format nil "insert ignore into posts (post_nr, board) values (~a, '~a');" post_nr board))
       (let ((post-id (caar (clsql:query (format nil "select id from posts where post_nr = ~a and board = '~a';" post_nr board)))))
         (clsql:execute-command
          (with-output-to-string (s)
            (format s "insert into postflags (post_nr, flag) values")
            (loop for flag in (butlast flags)
                  do (format s "(~a,~a)," post-id (flag-id flag)))
            (format s "(~a,~a);" post-id (flag-id (car (last flags))))))))

(dbfun get-posts (posts board)
       (let ((result (clsql:query (format nil get-posts-sql posts board)))
             (table (make-hash-table)))
         (loop for (post_nr . flag) in (reverse result) do
           (unless (gethash post_nr table)
             (setf (gethash post_nr table) '()))
           (push (car flag) (gethash post_nr table)))
         (jojo:to-json table)))

(dbfun get-flags ()
       (clsql:query "select flags.id, flags.flag from flags"))
