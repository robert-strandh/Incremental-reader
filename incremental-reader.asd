(cl:in-package #:asdf-user)

(defsystem incremental-reader
  :serial t
  :components
  ((:file "packages")
   (:file "stack")
   (:file "syntax-type")
   (:file "state")))
