(in-package :ray-tracing-in-one-week)

(defclass ray ()
  ((origin
    :initarg :origin
    :accessor origin
    :type vec3)
   (direction
    :initarg :direction
    :accessor direction
    :type vec3)))

(defun ray (orig dir)
  (make-instance 'ray :origin orig
		      :direction dir))

(defmethod at ((ray ray) s)
  (v+ (origin ray) (v* s (direction ray))))
