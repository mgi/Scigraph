(in-package :clim3-scigraph)

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

(defun zoom-in (plot)
  (with-accessors ((width clim3:width)
                   (height clim3:height)
                   (min-x plot-xmin)
                   (max-x plot-xmax)
                   (min-y plot-ymin)
                   (max-y plot-ymax)) plot
    (let* ((dx (- max-x min-x))
           (dy (- max-y min-y))
           (remove-x (/ dx 10))
           (remove-y (/ dy 10))
           (new-max-x (- max-x remove-x))
           (new-min-x (+ min-x remove-x))
           (new-max-y (- max-y remove-y))
           (new-min-y (+ min-y remove-y)))
      (setf min-x new-min-x
            max-x new-max-x
            min-y new-min-y
            max-y new-max-y))))

(defun run ()
  (let* ((port (clim3:make-port :clx-framebuffer))
         (title (clim3-text:text "Mon graphique" *text-style* *foreground*))
         (x-min (- (* 2 pi)))
         (x-max (* 2 pi))
         (graph-1 (make-graph *random* *foreground* 1))
         (graph-2 (make-graph *strange-attractor* (clim3:make-color 0 0 0) 0.1))
         (graph-3 (make-equation '(lambda (x) (sin x))
                                 x-min x-max (/ (- x-max x-min) 100)
                                 (clim3:make-color 1 0 0) 1.5))
         (graph-4 (make-graph *exp* (clim3:make-color 0 0.5 0) 1))
         (plot (make-grid-plot x-min x-max -1.5 1.5 (/ pi 4) .2 (list graph-3 graph-1 graph-4)))
         (plot2 (make-plot x-min x-max -1.5 1.5 (list graph-3)))
         (root (clim3:vbox* title plot)))
    (push-graph plot graph-2)
    (clim3:connect root port)
    (unwind-protect
         (let ((clim3:*port* port))
           (loop for keystroke = (clim3:read-keystroke)
                 do (case (car keystroke)
                      (#\+ (zoom-in plot) (clim3-ext:repaint port)))
                 until (eql (car keystroke) #\q)))
      (clim3:disconnect root port))))
