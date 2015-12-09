(cl:in-package #:asdf-user)

(defsystem incremental-reader
  :serial t
  :components
  ((:file "packages")
   (:file "stack")
   (:file "token")
   (:file "syntax-type")
   (:file "state")
   (:file "list-reader")))
