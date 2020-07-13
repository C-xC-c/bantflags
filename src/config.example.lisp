(in-package #:bantflags)

;; Change your configuration here after the `.'
(defparameter config
  '((:port             . 4242)
    (:boards           . ("bant" "uhh"))
    (:db-conn          . ("localhost" "bantflags" "flags" "default"))
    (:poolsize         . 3)))

;; Ignore this
(defmacro config-item (thing &aux (item (gensym)))
  `(let ((,item (assoc ,thing config)))
     (when (atom ,item)
       (error "No such config item"))
     (cdr ,item)))
