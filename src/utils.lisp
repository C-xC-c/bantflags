;; (C) Copyright 2020 C-xC-c <boku@plum.moe>
;; This file is part of bantflags.
;; bantflags is licensed under the GNU AGPL Version 3.0 or later.
;; see the LICENSE file or <https://www.gnu.org/licenses/>
(in-package #:bantflags)

(defvar empty-flag '("empty, or there were errors. Re-set your flags."))

(defun conf (thing)
  (let ((item (cdr (assoc thing config))))
    (if (null item)
        (error "no such config item" thing)
        item)))

(defun cconf (thing)
  (car (conf thing)))

;; db
(defun set-boards ()
  (setf *boards* (make-hash-table :test 'equal))
  (mapc (lambda (board) (setf (gethash board *boards*) t)) (conf 'boards)))

(defun set-flags ()
  (setf *flags* (make-hash-table :test 'equal))
  (let ((flags (get-flags)))
    (loop for (id . flag) in flags
          do (setf (gethash (car flag) *flags*) id))
    ;; We don't want users to select `empty-flag`
    (setf *flags-txt*
          (cl-ppcre:regex-replace (concatenate 'string (car empty-flag) "\\n") ;; newline
                                  (format nil "~{~a~^~%~}" (mapcan (lambda (x) (cdr x)) flags))
                                  ""))))

;; validation
(defun post-number-p (post_nr)
  (if (or (null post_nr)
          (null (parse-integer post_nr :junk-allowed t)))
      nil
      post_nr))

(defun boardp (board)
  (gethash board *boards*))

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

;; Content types
(defvar @json "application/json")
(defvar @plain "text/plain")
