(in-package :scigraph)

(defparameter *foreground* (clim3:make-color 0.4d0 0.3d0 0.8d0))
(defparameter *text-style* (clim3:text-style :free :sans :roman 20))

(defparameter *b-outer*
  '(((1 . 1) (4 . 1) (5.5 . 2) (6 . 3.5) (5.5 . 5) (4.5 . 5.5)
     (5.5 . 6) (6 . 7.5) (5.5 . 9) (4 . 10) (1 . 10))))

(defparameter *data* (loop for x from -200 to 200 collect (list x (expt x 2))))

(defparameter *strange-attractor*
  (let* ((x .1) y
	 (a 3.9)
	 (size 2000)
	 (res (make-list size)))
    (dotimes (i size)
      (setq y (* a x (- 1 x)))
      (setq x (* a y (- 1 y)))
      (setf (nth i res) (list x y)))
    res))

(defun min-max (data)
  (loop for (x y) in data
        minimize x into min-x
        maximize x into max-x
        minimize y into min-y
        maximize y into max-y
        finally (return (values min-x min-y max-x max-y))))

(defun square (x y size)
  (let ((half (/ size 2)))
    (list
     (cons (- x half) (+ y half))
     (cons (+ x half) (+ y half))
     (cons (+ x half) (- y half))
     (cons (- x half) (- y half)))))

(defun scale (x min max size)
  (* (- x min) (/ size (- max min))))

(defclass plot-zone (clim3:monochrome)
  ((%data :initarg :data :reader plot-zone-data))
  (:default-initargs :hsprawl (clim3-sprawl:sprawl 200 200 nil)
		     :vsprawl (clim3-sprawl:sprawl 200 200 nil)))

(defmethod clim3-ext:paint ((zone plot-zone))
  (let ((width (clim3:width zone))
        (height (clim3:height zone))
        (color (clim3:color zone))
        (data (plot-zone-data zone)))
    (multiple-value-bind (min-x min-y max-x max-y) (min-max data)
      (let ((points (loop for (x y) in data
                          collect (square (scale x min-x max-x width)
                                          (scale y max-y min-y height)
                                          1))))
        (clim3:with-area (0 0 width height)
          (clim3:paint-polygons points color))))))

(defun make-plot (data color)
  (make-instance 'plot-zone :data data :color color))

(defun run ()
  (let* ((port (clim3:make-port :clx-framebuffer))
         (title (clim3-text:text "Mon graphique" *text-style* *foreground*))
         (plot (make-plot *data* *foreground*))
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
