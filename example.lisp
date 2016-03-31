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
    (push-graph graph-1 *plot*)
    (push-graph graph-3 *plot*)
    (push-graph graph-4 *plot*)
    (clim3:connect root port)
    (unwind-protect
         (let ((clim3:*port* port))
           (catch :quit
             (clim3:command-loop)))
      (clim3:disconnect root port))))


(defun hsv2rgb (h s v)
  (let* ((chroma (* s v))
         (h* (/ h 60))
         (x (* chroma (- 1 (abs (- (mod h* 2) 1)))))
         (m (- v chroma)))
    (multiple-value-bind (r1 g1 b1)
        (cond  ((and (<= 0 h*) (< h* 1))
                (values chroma x 0))
               ((and (<= 1 h*) (< h* 2))
                (values x chroma 0))
               ((and (<= 2 h*) (< h* 3))
                (values 0 chroma x))
               ((and (<= 3 h*) (< h* 4))
                (values 0 x chroma))
               ((and (<= 4 h*) (< h* 5))
                (values x 0 chroma))
               ((and (<= 5 h*) (< h* 6))
                (values chroma 0 x))
               (t (values 0 0 0)))
      (values (+ r1 m) (+ g1 m) (+ b1 m)))))

(defun doplot (&rest data)
  (labels ((min-max-graphs (graphs)
             (let (min-xs max-xs min-ys max-ys)
               (dolist (gr graphs)
                 (multiple-value-bind (min-x max-x min-y max-y) (min-max gr)
                   (push min-x min-xs)
                   (push max-x max-xs)
                   (push min-y min-ys)
                   (push max-y max-ys)))
               (values (apply #'min min-xs)
                       (apply #'max max-xs)
                       (apply #'min min-ys)
                       (apply #'max max-ys)))))
    (let* ((port (clim3:make-port :clx-framebuffer))
           (n (length data))
           (graphs (loop with s = 1
                         for h from 0 below 360 by (/ 360 n)
                         for v from (/ 1 10) to 1 by (/ n)
                         for d in data
                         collect (make-graph d (multiple-value-call #'clim3:make-color (hsv2rgb h s v)) 1)))
           (*plot* (multiple-value-bind (xmin xmax ymin ymax) (min-max-graphs graphs)
                     (make-grid-plot xmin xmax ymin ymax (/ (- xmax xmin) 20) (/ (- ymax ymin) 20))))
           (root (clim3:vbox* *plot*))
           (view (make-instance 'view :command-key-processor (make-command-processor)))
           (clim3:*application* (make-instance 'scigraph :current-view view)))
      (dolist (gr graphs) (push-graph gr *plot*))
      (clim3:connect root port)
      (unwind-protect
           (let ((clim3:*port* port))
             (catch :quit
               (clim3:command-loop)))
        (clim3:disconnect root port)))))
