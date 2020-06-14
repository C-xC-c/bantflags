;; This needs to be run with a lisp started in the same directory as
;; bantflags
(push (truename ".") ql:*local-project-directories*)
(ql:register-local-projects)
(ql:quickload :bantflags)

(bantflags:main)
(hunchentoot:start bantflags:*serb*)
(loop (sleep 43200) (gc :full t))
