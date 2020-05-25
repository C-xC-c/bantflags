;; (C) Copyright 2020 C-xC-c <boku@plum.moe>
;; This file is part of bantflags.
;; bantflags is licensed under the GNU AGPL Version 3.0 or later.
;; see the LICENSE file or <https://www.gnu.org/licenses/>

(asdf:defsystem #:bantflags
  :description "the bantflags server component"
  :author "Manx (boku@plum.moe)"
  :mailto "boku@plum.moe"
  :license "AGPLv3+"
  :version "0.0.1"
  :serial t
  :depends-on (:hunchentoot
               :cl-ppcre
               :clsql
               :jonathan)
  :Components
  ((:file "utils")
   (:file "db")
   (:file "config")
   (:file "main")))
