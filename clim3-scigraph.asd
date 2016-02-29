(cl:in-package #:asdf-user)

(defsystem :clim3-scigraph
  :depends-on (:climatis)
  :serial t
  :components
  ((:file "package")
   (:file "graph")
   (:file "equation")
   (:file "plot")
   (:file "example")))
