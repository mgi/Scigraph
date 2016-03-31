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

(defun xy-form-p (list)
  (loop for e in list always (consp e)))

(defun y-form-p (list)
  (loop for e in list always (numberp e)))

(defun to-xy-form (list)
  (cond ((xy-form-p list) list)
        ((y-form-p list) (loop for x from 0
                               for y in list
                               collect (cons x y)))
        (t (error "~a is not a valid data form" list))))

(defun make-graph (data color thickness)
  (let ((data (to-xy-form data)))
    (make-instance 'graph :data (make-array (length data)
                                            :adjustable t
                                            :fill-pointer t
                                            :initial-contents data)
                          :color color :thickness thickness)))

(defgeneric min-max (object)
  (:documentation "Returns min-x max-x min-y max-y of a graph"))

(defmethod min-max ((self raw-graph))
  (with-accessors ((data data)) self
    (loop for point across data
          minimize (car point) into min-x
          minimize (cdr point) into min-y
          maximize (car point) into max-x
          maximize (cdr point) into max-y
          finally (return (values min-x max-x min-y max-y)))))

(defgeneric update-data (object))
(defmethod update-data ((self raw-graph)))
