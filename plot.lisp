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

(defun map-clip-scale (min-x max-x min-y max-y width height list)
  (loop for e in list
        when (clip-scale e min-x max-x min-y max-y width height)
          collect it))

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
    (clim3:with-area (0 0 width height)
      (dolist (graph graphs)
        (with-accessors ((data data)
                         (color color)
                         (thickness thickness)) graph
          (let ((scaled (clip-scale data min-x max-x min-y max-y width height)))
            (clim3:paint-paths (list scaled) color thickness)))))))

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
      (let ((color (clim3:make-color 0.2 0.2 0.2))
            (axes (map-clip-scale min-x max-x min-y max-y width height
                                  `(((,min-x . 0) (,max-x . 0))
                                    ((0 . ,min-y) (0 . ,max-y)))))
            grid)
        ;; ]0 .. x-max]
        (do ((x step-x (+ x step-x)))
            ((> x max-x))
          (push (list (cons x min-y) (cons x max-y)) grid))
        ;; ]0 .. x-min]
        (do ((x (- step-x) (- x step-x)))
            ((< x min-x))
          (push (list (cons x min-y) (cons x max-y)) grid))
        ;; ]0 .. y-max]
        (do ((y step-y (+ y step-y)))
            ((> y max-y))
          (push (list (cons min-x y) (cons max-x y)) grid))
        ;; ]0 .. y-min]
        (do ((y (- step-y) (- y step-y)))
            ((< y min-y))
          (push (list (cons min-x y) (cons max-x y)) grid))
        (setf grid (map-clip-scale min-x max-x min-y max-y width height grid))
        (clim3:paint-paths axes color 1.5)
        (clim3:paint-paths grid color 0.5)))))
