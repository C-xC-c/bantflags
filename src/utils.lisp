;; (C) Copyright 2020 C-xC-c <boku@plum.moe>
;; This file is part of bantflags.
;; bantflags is licensed under the GNU AGPL Version 3.0 or later.
;; see the LICENSE file or <https://www.gnu.org/licenses/>
(in-package #:bantflags)

(defvar empty-flag '("empty, or there were errors. Re-set your flags.")
  "Hardcoded flag for when things break. It's a list here since it
  makes insert-post-p less complex")

(defun set-boards ()
  (setf *boards* (make-hash-table :test 'equal))
  (mapc (lambda (board) (setf (gethash board *boards*) t)) (config-item :boards)))

(defun set-flags ()
  (setf *flags* (make-hash-table :test 'equal))
  (let ((flags (get-flags)))
    (loop for (id . flag) in flags
          do (setf (gethash (car flag) *flags*) id))
    (setf *flags-txt* ;; We don't want users to select `empty-flag`
          (cl-ppcre:regex-replace (concatenate 'string (car empty-flag) "\\n") ;; newline
                                  (format nil "~{~a~^~%~}" (mapcan (lambda (x) (cdr x)) flags))
                                  ""))))

;; validation
(defun post-number-p (post_nr)
  (unless (or (null post_nr)
              (null (parse-integer post_nr :junk-allowed t)))
    post_nr))

(defun boardp (board)
  "Returns the board if it exists in `*boards*', else nil"
  (gethash board *boards*))

;; Rewrite to return string or list -> typecase
(defun insert-post-p (post_nr regions board)
  (cond
    ((not (post-number-p post_nr))
     (values nil "Invalid post number."))
    ((not (boardp board))
     (values nil "Invalid board parameter."))
    ((null regions)
     (values t empty-flag))
    ((< 30 (length regions))
     (values nil "Too many flags."))
    ((every (lambda (flag) (gethash flag *flags*)) regions)
     (values t regions))
    (t (values t empty-flag))))

(defun get-posts-p (post_nrs board)
  (and (not (null post_nrs))
       (every #'post-number-p post_nrs)
       (boardp board)))

;; For temp files
(defun copy-stream (from to &key (buffer-size 4096)
                    &aux (buffer (make-array buffer-size :element-type '(unsigned-byte 8))))
  (loop for bytes-read = (read-sequence buffer from)
        while (plusp bytes-read)
        do (write-sequence buffer to :end bytes-read)))

(defun real-copy-file (from to)
  (with-open-file (from from :direction :input
                             :element-type '(unsigned-byte 8))
    (with-open-file (to to :direction :output
                           :if-exists :supersede
                           :element-type '(unsigned-byte 8))
      (copy-stream from to))))

(defun real-move-file (from to)
  (real-copy-file from to)
  (delete-file from))
