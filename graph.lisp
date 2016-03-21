(in-package :clim3-scigraph)

(defclass raw-graph (clim3:wrap)
  ((%data :initarg :data :accessor data))
  (:default-initargs :hsprawl (clim3-sprawl:sprawl 800 800 nil)
                     :vsprawl (clim3-sprawl:sprawl 600 600 nil)))

(defclass graph-style-mixin ()
  ((%thickness :initform 1 :initarg :thickness :accessor thickness)
   (%color :initform (clim3:make-color 0 0 0) :initarg :color :accessor color)))

(defclass simple-statistics-mixin () ()
  (:documentation "Provide some commonly used statistical metrics."))

(defclass graph (raw-graph graph-style-mixin simple-statistics-mixin) ())

(defun make-graph (data color thickness)
  (make-instance 'graph :data (make-array (length data)
                                          :adjustable t
                                          :fill-pointer t
                                          :initial-contents data)
                        :color color :thickness thickness))
