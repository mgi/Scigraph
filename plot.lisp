(in-package :clim3-scigraph)

(defun filter-scale (points min-x max-x min-y max-y width height)
  (let ((dw (/ width (- max-x min-x)))
        (dh (/ height (- min-y max-y)))
        last-out last-in res)
    (flet ((inside-p (p)
             (let ((x (car p))
                   (y (cdr p)))
               (and (>= x 0) (<= x width)
                    (>= y 0) (<= y height))))
           (scale (p) (cons (* (- (car p) min-x) dw)
                            (* (- (cdr p) max-y) dh))))
      (dolist (p points (reverse res))
        (let ((sp (scale p)))
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

(defclass plot-zone (clim3:monochrome)
  ((%graphs :initarg :graphs :accessor plot-zone-graphs)
   (%xmin :initarg :xmin :accessor plot-zone-xmin)
   (%xmax :initarg :xmax :accessor plot-zone-xmax)
   (%ymin :initarg :ymin :accessor plot-zone-ymin)
   (%ymax :initarg :ymax :accessor plot-zone-ymax))
  (:default-initargs :hsprawl (clim3-sprawl:sprawl 800 800 nil)
		     :vsprawl (clim3-sprawl:sprawl 600 600 nil)))

(defmethod clim3-ext:paint ((zone plot-zone))
  (with-accessors ((width clim3:width)
                   (height clim3:height)
                   (graphs plot-zone-graphs)
                   (min-x plot-zone-xmin)
                   (max-x plot-zone-xmax)
                   (min-y plot-zone-ymin)
                   (max-y plot-zone-ymax)) zone
    (dolist (graph graphs)
      (with-accessors ((data data)
                       (color color)
                       (thickness thickness)) graph
        (clim3:with-area (0 0 width height)
          (clim3:paint-path (filter-scale data min-x max-x min-y max-y width height)
                            color thickness))))))

(defun make-plot (xmin xmax ymin ymax &optional graphs)
  (make-instance 'plot-zone :xmin xmin
                            :xmax xmax
                            :ymin ymin
                            :ymax ymax
                            :graphs graphs))
