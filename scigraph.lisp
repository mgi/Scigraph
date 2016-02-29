(in-package :scigraph)

(defparameter *foreground* (clim3:make-color 0.4d0 0.3d0 0.8d0))
(defparameter *text-style* (clim3:text-style :camfer :sans :roman 20))

(defparameter *exp* (loop for x from -20 to 20 by 0.1 collect (cons x (expt x 2))))
(defparameter *sin* (loop for x from (* -2 pi) to (* 2 pi) by (/ pi 20) collect (cons x (sin x))))
(defparameter *random* (loop for x below 50 collect (cons x (random (1+ x)))))

(defparameter *strange-attractor*
  (let* ((x .1) y
	 (a 3.9)
	 (size 1000)
	 (res (make-list size)))
    (dotimes (i size)
      (setq y (* a x (- 1 x)))
      (setq x (* a y (- 1 y)))
      (setf (nth i res) (cons x y)))
    res))

(defun min-max (data)
  (loop for (x . y) in data
        minimize x into min-x
        maximize x into max-x
        minimize y into min-y
        maximize y into max-y
        finally (return (values min-x min-y max-x max-y))))

(defun scale (x min max size)
  (* (- x min) (/ size (- max min))))

(defun filter-scale (points min-x max-x min-y max-y width height)
  (loop with dw = (/ width (- max-x min-x))
        with dh = (/ height (- min-y max-y))
        for p in points
        when (let* ((x (car p))
                    (y (cdr p))
                    (sx (* (- x min-x) dw))
                    (sy (* (- y max-y) dh)))
               (when (and (>= sx 0)
                          (<= sx width)
                          (>= sy 0)
                          (<= sy height))
                 (cons sx sy)))
          collect it))

(defclass graph ()
  ((%data :initarg :data :reader graph-data)
   (%color :initarg :color :reader graph-color)
   (%stroke-width :initarg :stroke-width :reader graph-stroke-width)
   (%legend :initarg :legend :reader graph-legend)))

(defclass plot-zone (clim3:monochrome)
  ((%graphs :initarg :graphs :accessor plot-zone-graphs)
   (%xmin :initarg :xmin :accessor plot-zone-xmin)
   (%xmax :initarg :xmax :accessor plot-zone-xmax)
   (%ymin :initarg :ymin :accessor plot-zone-ymin)
   (%ymax :initarg :ymax :accessor plot-zone-ymax))
  (:default-initargs :hsprawl (clim3-sprawl:sprawl 800 800 nil)
		     :vsprawl (clim3-sprawl:sprawl 600 600 nil)))

(defclass path-zone (clim3:monochrome)
  ((%path :initarg :path :accessor path-zone-path))
  (:default-initargs :hsprawl (clim3-sprawl:sprawl 300 300 nil)
                     :vsprawl (clim3-sprawl:sprawl 300 300 nil)))

(defmethod clim3-ext:paint ((zone plot-zone))
  (with-accessors ((width clim3:width)
                   (height clim3:height)
                   (graphs plot-zone-graphs)
                   (min-x plot-zone-xmin)
                   (max-x plot-zone-xmax)
                   (min-y plot-zone-ymin)
                   (max-y plot-zone-ymax)) zone
    (dolist (graph graphs)
      (with-accessors ((data graph-data)
                       (color graph-color)
                       (stroke-width graph-stroke-width)
                       (legend graph-legend)) graph
        (clim3:with-area (0 0 width height)
          (clim3:paint-path (filter-scale data min-x max-x min-y max-y width height)
                            color stroke-width))))))

(defmethod clim3-ext:paint ((zone path-zone))
  (with-accessors ((width clim3:width)
                   (height clim3:height)
                   (color clim3:color)
                   (path path-zone-path)) zone
    (when path
      (clim3:with-area (0 0 width height)
        (clim3:paint-path path color 1.0)))))

(defun make-graph (data color stroke-width legend)
  (make-instance 'graph :data data :color color :stroke-width stroke-width :legend legend))

(defun make-plot (xmin xmax ymin ymax &optional graphs)
  (make-instance 'plot-zone :xmin xmin
                            :xmax xmax
                            :ymin ymin
                            :ymax ymax
                            :graphs graphs))

(defun run ()
  (let* ((port (clim3:make-port :clx-framebuffer))
         (title (clim3-text:text "Mon graphique" *text-style* *foreground*))
         (graph-1 (make-graph *random* *foreground* 1.0 "Random"))
         (graph-2 (make-graph *strange-attractor* (clim3:make-color 0 0 0) 0.1 "Strange attractor"))
         (graph-3 (make-graph *sin* (clim3:make-color 1 0 0) 1.5 "Sinus"))
         (graph-4 (make-graph *exp* (clim3:make-color 0 0.5 0) 1.0 "Exponentielle"))
         (plot (make-plot (- (* 2 pi)) (* 2 pi) -1.5 1.5 (list graph-3 graph-1 graph-4)))
         (root (clim3:vbox* title plot)))
    (clim3:connect root port)
    (unwind-protect
         (let ((clim3:*port* port))
           (loop for keystroke = (clim3:read-keystroke)
                 until (eql (car keystroke) #\q)))
      (clim3:disconnect root port))))

(defun test-path ()
  (let* ((port (clim3:make-port :clx-framebuffer))
         (extreme-path (loop for y = 10000 then (- y)
                             for x below 600 by 100 collect (cons x y)))
         (zone (make-instance 'path-zone :path extreme-path
                                         :color (clim3:make-color 0 0 0)))
         (root (clim3:vbox* zone)))
    (clim3:connect root port)
    (unwind-protect
         (let ((clim3:*port* port))
           (loop for keystroke = (clim3:read-keystroke)
                 until (eql (car keystroke) #\q)))
      (clim3:disconnect root port))))

(defun display-font (foundry family face size &optional string)
  (let* ((style (clim3:text-style foundry family face size))
         (str (if string
                  string
                  (clim3-fonts:glyphs-string (clim3-fonts:text-style-to-font style))))
         (port (clim3:make-port :clx-framebuffer))
         (text (clim3-gadgets:text str))
         (root (clim3:vbox* text
                            (clim3-gadgets:button text nil)
                            (clim3-gadgets:butcon text nil))))
    (clim3:connect root port)
    (let ((clim3:*port* port))
      (loop for keystroke = (clim3:read-keystroke)
            until (eql (car keystroke) #\q)))
    (clim3:disconnect root port)))
