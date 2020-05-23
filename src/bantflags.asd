(asdf:defsystem #:bantflags
  :description "the bantflags server component"
  :author "Manx (boku@plum.moe)"
  :mailto "boku@plum.moe"
  :license "AGPLv3+"
  :version "0.0.1"
  :serial t
  :depends-on (:hunchentoot
               :str
               :cl-dbi
               :jonathan)
  :Components
  ((:file "utils")
   (:file "db")
   (:file "config")
   (:file "main")))
