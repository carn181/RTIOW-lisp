(in-package :ray-tracing-in-one-week)

(defparameter *image-file* "render.ppm")



(defun write-color (s v)
  (format s "~D ~D ~D~%"
	  (round (* 255 (vx v)))
	  (round (* 255 (vy v)))
	  (round (* 255 (vz v)))))

(defmacro render-pixels (w h &body body)
  `(with-open-file (f *image-file*
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create)
     (format f "P3~%~D ~D~%255~%" ,w ,h)
     
     (loop for j from (1- ,h) downto 0
	   do (loop for i from 0 upto (1- ,w)
		    do (progn ,@body)))
     (format t "~%Done !")))


				


(defun 1r-gradient-image ()
  (let ((iw 400)
	(ih 400))
    (render-pixels iw ih
      (let ((r (/ j iw))
	    (g (/ i ih))
	    (b 0.25))
	(format f "~D ~D ~D%"
	  (round (* 255.999 r))
	  (round (* 255.999 g))
	  (round (* 255.999 b)))))))

(defun color-pixel-2r (r)
  (let* ((unit-direction (vunit (direction r)))
	 (tb (* 0.5
		(+ 1 (vy unit-direction))))
	 (tc (hit-sphere (vec3 0 0 -1) 0.6 r)))
    (if (> tc 0)
	(progn
	  (let ((N (vunit (v- (at r tc)
			      (vec3 0 0 -1)))))
	    (v* 0.5
		(vec3 (1+ (vy N))
		      (1+ (vz N))
		      (1+ (vx N))))))
	(progn (v+ (v* (- 1.0 tb)
		       (vec3 1 1 1))
		   (v* tb
		       (vec3 0.2 0.7 1)))))))

(defun hit-sphere (center radius r)
  ;; If ray hits sphere, returns t required for ray to reach point on sphere
  ;; This t is used to find normal vector on that point
  ;; Otherwise -1
  (let* ((oc (v- (origin r)
		 center))
	 (rd (direction r))
	 (a (v. rd rd))
	 (b (* 2 (v. oc rd)))
	 (c (- (v. oc oc)
		(* radius radius)))
	 (discriminant (- (* b b)
			  (* 4 a c))))
    (if (> discriminant 0)
	(/ (- (* b -1)
	      (sqrt discriminant))
	   (* 2 a))
	-1)))

(defun 2r-viewport(&optional debug)
  (let* ((aspect-ratio (/ 16 9))
	 ;; Image
	 (iw 400)                   ;; Image Width
	 (ih (/ iw aspect-ratio))   ;; Image Height
	 ;; Camera
	 (origin (vec3 0 0 0))
	 (fl 1)                     ;; Focal Length
	 (vh 2)                     ;; Viewport Height
	 (vw (* aspect-ratio vh))   ;; Viewport Width
	 (horizontal (vec3 vw 0 0))
	 (vertical   (vec3 0  vh 0))
	 (lower-left-corner         ;; Vector of Lower Left Corner
	   (v- origin
	       (v/ horizontal 2)
	       (v/ vertical 2)
	       (vec3 0 0 fl))))
    (render-pixels iw ih
      (let* ((u (/ i
		   iw))
	     (v (/ j
		   ih))
	     (r (ray origin
		     (v- (v+ lower-left-corner ;; Scaling Camera's View to Viewport
			     (v* u horizontal)
			     (v* v vertical))
			 origin))))
	(if debug
	    (progn (print (float (/ j (1- ih))))))
	(write-color f (color-pixel-2r r))))))

(defclass hit-record ()
  ((p :initarg :p :accessor p :type vec3)
   (normal :initarg :normal :accessor normal :type vec3)
   (ray-t :initarg :ray-t :accessor ray-t :type double-float)
   (front-face :initarg :front-face :accessor front-face :type boolean)))

(defmethod set-face-normal ((h hit-record) ray outward-normal)
  (let* ((front-face (> (v. (direction ray) outward-normal) 0))
	 (normal (if front-face
		     outward-normal
		     (v* -1 outward-normal))))
    (setf (front-face h) front-face)
    (setf (normal h) normal)))

(defclass hittable () ())

(defmethod hit ((h hittable) r ray-tmin ray-tmax))

(defclass sphere (hittable)
  ((radius :initarg :radius :accessor radius :type double-float)
   (center :initarg :center :accessor center :type vec3)))

(defun valid-root (a b sqrt-d tmin tmax)
  (let ((root (/ (- (* b -1)
		     sqrt-d)
		  (* 2 a))))
    (if (and (<= tmin root) (>= tmax root))
	root
	(progn (setf root (/ (+ (* b -1)
				sqrt-d)
			     (* 2 a)))
	       (if (and (<= tmin root) (>= tmax root))
		   root
		   nil)))))

(defmethod hit ((s sphere) r tmin tmax)
  (let* ((radius (radius s))
	 (center (center s))
	 (oc (v- (origin r)
		 center))
	 (rd (direction r))
	 (a (v. rd rd))
	 (b (* 2 (v. oc rd)))
	 (c (- (v. oc oc)
		(* radius radius)))
	 (discriminant (- (* b b)
			  (* 4 a c))))
    (if (< discriminant 0)
	nil
	(let ((root (valid-root a b (sqrt discriminant) tmin tmax)))
	  (if (typep root 'boolean)
	      nil
	      (let* ((hit-point (at r root))
		     (normal (vunit (v- (at r root) center)))
		     (hit-record (make-instance 'hit-record
						:front-face nil
						:ray-t root
						:normal normal
						:p hit-point)))
		(set-face-normal hit-record r normal)
		(values t hit-record)
		))))))

(defclass hittable-list (hittable)
  ((objects :accessor objects :initform nil)))

(defmethod add ((h hittable-list) object)
  (push object (objects h)))

(defmethod clear ((h hittable-list) object)
  (setf object '()))

(defmethod hit ((h hittable-list) r tmin tmax)
  (let ((temp-hit-record nil)
	(hit-anything nil)
	(closest-so-far tmax))
    (loop for obj in (objects h)
	  do (multiple-value-bind (hitp hit-record) (hit obj r tmin closest-so-far)
	       (if hitp
		   (progn
		     (setf hit-anything t)
		     (setf closest-so-far (ray-t hit-record))
		     (setf temp-hit-record hit-record)))))
    (values hit-anything temp-hit-record)))

(defvar *infinity* 1.7976931348623158D308)

(defun color-pixel (r world)
  (multiple-value-bind (hit-anything hit-record) (hit world r 0 *infinity*)
    (if hit-anything
	(let ((N (normal hit-record)))
	  (v* 0.5
	      (vec3 (1+ (vy N))
		    (1+ (vz N))
		    (1+ (vx N)))))
	(let* ((unit-direction (vunit (direction r)))
	       (a (* 0.5 (1+ (vy unit-direction)))))
	  (v+ (v* (- 1 a)
		  (vec3 1.0 1.0 1.0))
	      (v* a
		  (vec3 0.5 0.7 1.0)))))))
			
(defun my-random (n)
    (- (random (1+ (* 2 n))) n))

(defun 3r-hittables(&optional debug)
  (let* ((aspect-ratio (/ 16 9))
	 ;; Image
	 (iw 400)                   ;; Image Width
	 (ih (/ iw aspect-ratio))   ;; Image Height
	 ;; Camera
	 (origin (vec3 0 0 5))
	 (fl 1)                     ;; Focal Length
	 (vh 2)                     ;; Viewport Height
	 (vw (* aspect-ratio vh))   ;; Viewport Width
	 (horizontal (vec3 vw 0 0))
	 (vertical   (vec3 0  vh 0))
	 (lower-left-corner         ;; Vector of Lower Left Corner
	   (v- origin
	       (v/ horizontal 2)
	       (v/ vertical 2)
	       (vec3 0 0 fl)))
	 (world (make-instance 'hittable-list)))
    (setf *random-state* (make-random-state))
;;    (add world (make-instance 'sphere :center (vec3 1 0 -1) :radius 0.3))
    (add world (make-instance 'sphere :center (vec3 0 -103.5 -1) :radius 100))
    (loop repeat 30
	  do 
	     (add world (make-instance 'sphere
				       :center (vec3 (my-random 3.0)
						     (my-random 3.0)
						     (my-random 3.0))
				       :radius (random 1.0))))
    (render-pixels iw ih
      (let* ((u (/ i
		   iw))
	     (v (/ j
		   ih))
	     (r (ray origin
		     (v- (v+ lower-left-corner ;; Scaling Camera's View to Viewport
			     (v* u horizontal)
			     (v* v vertical))
			 origin))))
	(if debug
	    (progn (print (float (/ j (1- ih))))))
	(write-color f (color-pixel r world))))))
