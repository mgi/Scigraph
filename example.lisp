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
         (plot (make-plot x-min x-max -1.5 1.5 (list graph-3 graph-1 graph-4)))
         (root (clim3:vbox* title plot)))
    (clim3:connect root port)
    (unwind-protect
         (let ((clim3:*port* port))
           (loop for keystroke = (clim3:read-keystroke)
                 until (eql (car keystroke) #\q)))
      (clim3:disconnect root port))))
