(in-package :clim3-scigraph)

(defclass view ()
  ((%command-key-processor :initarg :command-key-processor
                           :reader clim3:command-table
                           :accessor command-key-processor)))

(defmethod clim3:acquire-action :around ((view view))
  (let ((clim3:*key-handler*
	  (make-instance 'clim3:key-processor-key-handler
	    :processor (command-key-processor view))))
    (call-next-method)))
