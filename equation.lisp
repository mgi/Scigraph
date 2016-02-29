(in-package :clim3-scigraph)

(defclass equation (graph)
  ((%eqn :initarg :equation :reader equation)
   (%fun :initarg :func :reader func)
   (%min :initform 0 :initarg :min :reader minimum)
   (%max :initform 100 :initarg :max :reader maximum)
   (%increment :initform 1 :initarg :increment :reader increment)))

(defmethod initialize-instance :after ((object equation) &key &allow-other-keys)
  (with-accessors ((func func)
                   (min minimum)
                   (max maximum)
                   (increment increment)
                   (data data)) object
    (setf data (loop for x from min to max by increment
                     collect (cons x (funcall func x))))))

(defun make-equation (lambda-equation min max increment color thickness)
  (make-instance 'equation
                 :equation lambda-equation
                 :func (compile nil lambda-equation)
                 :min min
                 :max max
                 :increment increment
                 :color color
                 :thickness thickness))
