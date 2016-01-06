(cl:in-package #:asdf-user)

(defsystem :scigraph
  :depends-on (:climatis)
  :serial t
  :components
  ((:file "package")
   (:file "scigraph")))
