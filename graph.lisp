(in-package :clim3-scigraph)

(defclass raw-graph ()
  ((%data :initarg :data :accessor data)))

(defclass graph-style-mixin ()
  ((%thickness :initform 1 :initarg :thickness :accessor thickness)
   (%color :initform (clim3:make-color 0 0 0) :initarg :color :accessor color)))

(defclass simple-statistics-mixin () ()
  (:documentation "Provide some commonly used statistical metrics."))

(defclass graph (raw-graph graph-style-mixin simple-statistics-mixin) ())

(defun make-graph (data color thickness)
  (make-instance 'graph :data data :color color :thickness thickness))
