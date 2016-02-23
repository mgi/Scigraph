(in-package :scigraph)

(defparameter *foreground* (clim3:make-color 0.4d0 0.3d0 0.8d0))
(defparameter *text-style* (clim3:text-style :camfer :sans :roman 20))

(defparameter *data* (loop for x from -20 to 20 collect (cons x (expt x 2))))
(defparameter *data2* (loop for x from (* -2 pi) to (* 2 pi) by (/ pi 20) collect (cons x (sin x))))

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

(defclass graph ()
  ((%data :initarg :data :reader graph-data)
   (%color :initarg :color :reader graph-color)
   (%stroke-width :initarg :stroke-width :reader graph-stroke-width)))

(defclass plot-zone (clim3:monochrome)
  ((%graphs :initarg :graphs :accessor plot-zone-graphs))
  (:default-initargs :hsprawl (clim3-sprawl:sprawl 800 800 nil)
		     :vsprawl (clim3-sprawl:sprawl 600 600 nil)))

(defmethod clim3-ext:paint ((zone plot-zone))
  (let ((width (clim3:width zone))
        (height (clim3:height zone))
        (graphs (plot-zone-graphs zone)))
    (dolist (graph graphs)
      (let ((data (graph-data graph))
            (color (graph-color graph))
            (stroke-width (graph-stroke-width graph)))
        (multiple-value-bind (min-x min-y max-x max-y) (min-max data)
          (clim3:with-area (0 0 width height)
            (let ((scaled-data (mapcar #'(lambda (p)
                                           (cons (scale (car p) min-x max-x width)
                                                 (scale (cdr p) min-y max-y height))) data)))
              (clim3:paint-path scaled-data color stroke-width))))))))

(defun make-graph (data color stroke-width)
  (make-instance 'graph :data data :color color :stroke-width stroke-width))

(defun make-plot (&optional graphs)
  (make-instance 'plot-zone :graphs (when (and graphs (listp graphs)) graphs)))

(defun run ()
  (let* ((port (clim3:make-port :clx-framebuffer))
         (title (clim3-text:text "Mon graphique" *text-style* *foreground*))
         (graph-1 (make-graph (loop for x below 50 collect (cons x (random (1+ x)))) *foreground* 1.0))
         (graph-2 (make-graph *strange-attractor* (clim3:make-color 0 0 0) 0.1))
         (graph-3 (make-graph *data2* (clim3:make-color 1 0 0) 1.0))
         (plot (make-plot (list graph-1 graph-2 graph-3)))
         (root (clim3:vbox* title plot)))
    (clim3:connect root port)
    (let ((clim3:*port* port))
      (loop for keystroke = (clim3:read-keystroke)
	    until (eql (car keystroke) #\q)))
    (clim3:disconnect root port)))

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
