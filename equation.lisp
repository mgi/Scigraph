(in-package :clim3-scigraph)

(defclass equation (graph)
  ((%eqn :initarg :equation :reader equation)
   (%fun :initarg :func :reader func)
   (%plot :initform nil :accessor plot)
   (%resolution :initarg :resolution :reader resolution)))

(defmethod update-data ((self equation))
  (with-accessors ((func func)
                   (resolution resolution)
                   (data data)
                   (plot plot)) self
    (when plot
      (with-accessors ((min-x plot-xmin)
                       (max-x plot-xmax)) plot
        (let ((dx (/ (- max-x min-x) resolution)))
          (setf data (loop for x from min-x below (+ max-x dx) by dx
                           collect (cons x (funcall func x)))))))))

(defun make-equation (lambda-equation color thickness &optional (resolution 100))
  (make-instance 'equation
                 :equation lambda-equation
                 :func (compile nil lambda-equation)
                 :resolution resolution
                 :color color
                 :thickness thickness))
