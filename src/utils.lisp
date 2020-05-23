(defvar empty-flag '("empty, or there were errors. Re-set your flags."))

(defun conf (thing)
  (let ((item (cdr (assoc thing config))))
    (if (null item)
        (error "no such config item" thing)
        item)))

(defun cconf (thing)
  (car (conf thing)))

(defun set-boards ()
  (setf *boards* (make-hash-table :test 'equal))
  (mapc (lambda (board) (setf (gethash board *boards*) t)) (conf 'boards)))

(defun set-flags ()
  (setf *flags* (make-hash-table :test 'equal))

  (let ((flags (get-flags)))
    (loop for (id . flag) in flags
          do (setf (gethash (car flag) *flags*) id))
    (setf *flags-txt*
          (cl-ppcre:regex-replace "empty, or there were errors. Re-set your flags\\.\\n"
                                  (format nil "~{~a~^~%~}" (mapcan (lambda (x) (cdr x)) flags))
                                  ""))))
(defun set-db-conn ()
  (setq conn-str (conf 'db-conn)))

(defun get-version (thing)
  (if (null thing) 0
      (or (parse-integer thing :junk-allowed t) 0)))

(defun post-number-p (post_nr)
  (if (or (null post_nr)
          (null (parse-integer post_nr :junk-allowed t)))
      nil
      post_nr))

(defun boardp (board)
  (gethash board *boards*))

(defun post-valid-p (post_nr regions board separator)
  (let ((flags (str:split separator regions)))
    (cond
      ((not (post-number-p post_nr))
       (values nil "Invalid post number"))
      ((not (boardp board))
       (values nil "Invalid board parameter."))
      ((null regions)
       (values t empty-flag))
      ((< 30 (length flags))
       (values nil "Too many flags."))
      ((loop for flag in flags
             always (gethash flag *flags*))
       (values t flags))
      (t (values t empty-flag)))))

(defun host-dir (uri path)
  (push
   (hunchentoot:create-folder-dispatcher-and-handler uri path)
   hunchentoot:*dispatch-table*))

;; This is uneccessarily complicated, no I'm not sorry
(defmacro content-type (types)
  (cons 'progn
        (mapcar (lambda (type) `(defun ,(car type) (reply)
                             (setf (tbnl:content-type* reply) ,(cadr type))))
                types)))
(content-type
 ((@json "application/json")
  (@plain "text/plain")))
