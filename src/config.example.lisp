(defvar config
  '((boards "bant")
    (staging-password "not implemented")
    (db-conn "localhost" "bantflags" "flags" "default")
    (poolsize 3)
    (www-root #p"/path/to/files/")
    (port 4242)
    ;; These can be a file or stream, make them nil to disable logging
    (access-log *standard-output*)
    (error-log #p"/path/to/error/log/")))
