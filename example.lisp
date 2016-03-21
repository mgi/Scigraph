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

(defvar *plot*)

(defclass scigraph (clim3:application)
  ((%current-view :initarg :current-view :accessor clim3:current-view)))

(defun run ()
  (let* ((port (clim3:make-port :clx-framebuffer))
         (title (clim3-text:text "Mon graphique" *text-style* *foreground*))
         (graph-1 (make-graph *random* *foreground* 4))
         (graph-2 (make-graph *strange-attractor* (clim3:make-color 0 0 0) 0.1))
         (graph-3 (make-equation '(lambda (x) (sin x)) (clim3:make-color 1 0 0) 1.5))
         (graph-4 (make-graph *exp* (clim3:make-color 0 0.5 0) 1))
         (*plot* (make-grid-plot (- (* 2 pi)) (* 2 pi) -1.5 1.5 (/ pi 4) .2))
         (root (clim3:vbox* title *plot*))
         (view (make-instance 'view :command-key-processor (make-command-processor)))
         (clim3:*application* (make-instance 'scigraph :current-view view)))
    (push-graph *plot* graph-1)
    (push-graph *plot* graph-3)
    (push-graph *plot* graph-4)
    (clim3:connect root port)
    (unwind-protect
         (let ((clim3:*port* port))
           (catch :quit
             (clim3:command-loop)))
      (clim3:disconnect root port))))
