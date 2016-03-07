(in-package :clim3-scigraph)

(defun zoom (plot &optional (in-p t))
  (with-accessors ((width clim3:width)
                   (height clim3:height)
                   (min-x plot-xmin)
                   (max-x plot-xmax)
                   (min-y plot-ymin)
                   (max-y plot-ymax)) plot
    (let* ((dx (- max-x min-x))
           (dy (- max-y min-y))
           (some-x (/ dx 10))
           (some-y (/ dy 10))
           (at-max (if in-p #'- #'+))
           (at-min (if in-p #'+ #'-))
           (new-max-x (funcall at-max max-x some-x))
           (new-min-x (funcall at-min min-x some-x))
           (new-max-y (funcall at-max max-y some-y))
           (new-min-y (funcall at-min min-y some-y)))
      (setf min-x new-min-x
            max-x new-max-x
            min-y new-min-y
            max-y new-max-y))))

(defclass scigraph-command-processor (clim3:command-table) ())

(defmethod clim3:submit-keystroke ((key-processor scigraph-command-processor) keystroke)
  (case (car keystroke)
    (#\+ (zoom *plot*))
    (#\- (zoom *plot* nil))
    (#\q (throw :quit nil))))

(defun make-command-processor ()
  (make-instance 'scigraph-command-processor))
