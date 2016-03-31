(in-package :clim3-scigraph)

(defun scale (x boundary size)
  (* (- x boundary) size))

(defun clip-scale (points min-x max-x min-y max-y width height)
  (let ((dw (/ width (- max-x min-x)))
        (dh (/ height (- min-y max-y)))
        last-out last-in res)
    (flet ((inside-p (p)
             (let ((x (car p))
                   (y (cdr p)))
               (and (>= x 0) (<= x width)
                    (>= y 0) (<= y height))))
           (scale-point (p) (cons (scale (car p) min-x dw)
                                  (scale (cdr p) max-y dh))))
      (dotimes (i (length points) (reverse res))
        (let ((sp (scale-point (elt points i))))
          (cond ((inside-p sp)
                 (when last-out
                   (push last-out res)
                   (setf last-out nil))
                 (push sp res)
                 (setf last-in sp))
                (t
                 (when last-in
                   (push sp res)
                   (setf last-in nil))
                 (setf last-out sp))))))))

(defclass plot (clim3:pile)
  ((%xmin :initarg :xmin :accessor plot-xmin)
   (%xmax :initarg :xmax :accessor plot-xmax)
   (%ymin :initarg :ymin :accessor plot-ymin)
   (%ymax :initarg :ymax :accessor plot-ymax)))

(defmethod clim3-ext:paint ((zone plot))
  (with-accessors ((width clim3:width)
                   (height clim3:height)
                   (graphs clim3:children)
                   (min-x plot-xmin)
                   (max-x plot-xmax)
                   (min-y plot-ymin)
                   (max-y plot-ymax)) zone
    (dolist (graph graphs)
      (with-accessors ((data data)
                       (color color)
                       (thickness thickness)) graph
        (clim3:with-area (0 0 width height)
          (clim3:paint-paths (list (clip-scale data min-x max-x min-y max-y width height))
                             color thickness))))))

(defmethod (setf plot-xmax) :after (value (zone plot))
  (with-accessors ((graphs clim3:children)) zone
      (dolist (graph graphs) (update-data graph))))

(defun make-plot (xmin xmax ymin ymax)
  (make-instance 'plot :xmin xmin
                       :xmax xmax
                       :ymin ymin
                       :ymax ymax))

(defgeneric push-graph (graph plot))

(defmethod push-graph ((graph graph) plot)
  (with-accessors ((graphs clim3:children)) plot
    (push graph graphs)))

(defmethod push-graph :after ((graph equation) plot)
  (setf (plot graph) plot)
  (update-data graph))

(defun pop-graph (plot)
  (with-accessors ((graphs clim3:children)) plot
    (pop graphs)))

(defclass grid-plot (plot)
  ((%xstep :initarg :xstep :accessor grid-plot-step-x)
   (%ystep :initarg :ystep :accessor grid-plot-step-y)))

(defun make-grid-plot (xmin xmax ymin ymax xstep ystep)
  (make-instance 'grid-plot :xmin xmin
                            :xmax xmax
                            :ymin ymin
                            :ymax ymax
                            :xstep xstep
                            :ystep ystep))

(defmethod clim3-ext:paint :before ((zone grid-plot))
  (with-accessors ((width clim3:width)
                   (height clim3:height)
                   (min-x plot-xmin)
                   (max-x plot-xmax)
                   (min-y plot-ymin)
                   (max-y plot-ymax)
                   (step-x grid-plot-step-x)
                   (step-y grid-plot-step-y)) zone
    (clim3:with-area (0 0 width height)
      (let* ((dw (/ width (- max-x min-x)))
             (dh (/ height (- min-y max-y)))
             (color (clim3:make-color 0.2 0.2 0.2))
             (sx0 (scale 0 min-x dw))
             (sy0 (scale 0 max-y dh))
             (axes (list (list (cons sx0 0)
                               (cons sx0 height))
                         (list (cons 0 sy0)
                               (cons width sy0))))
             grid)
        ;; ]0 .. x-max]
        (do ((x step-x (+ x step-x)))
            ((> x max-x))
          (let ((sx (scale x min-x dw)))
            (push (list (cons sx 0) (cons sx height)) grid)))
        ;; ]0 .. x-min]
        (do ((x (- step-x) (- x step-x)))
            ((< x min-x))
          (let ((sx (scale x min-x dw)))
            (push (list (cons sx 0) (cons sx height)) grid)))
        ;; ]0 .. y-max]
        (do ((y step-y (+ y step-y)))
            ((> y max-y))
          (let ((sy (scale y max-y dh)))
            (push (list (cons 0 sy) (cons width sy)) grid)))
        ;; ]0 .. y-min]
        (do ((y (- step-y) (- y step-y)))
            ((< y min-y))
          (let ((sy (scale y max-y dh)))
            (push (list (cons 0 sy) (cons width sy)) grid)))
        (clim3:paint-paths axes color 1.5)
        (clim3:paint-paths grid color 0.5)))))
