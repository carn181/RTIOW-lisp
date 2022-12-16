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

(defun color-pixel (r)
  (let* ((unit-direction (vunit (direction r)))
	 (tb (* 0.5
		(1+ (vy unit-direction))))
	 (tc (hit-sphere (vec3 0 0 -1) 0.5 r)))
    (if (> tc 0)
	(progn
	  (let ((N (vunit (v- (at r tc)
			      (vec3 0 0 -1)))))
	    (v* 0.5
		(vec3 (1+ (vx N))
		      (1+ (vy N))
		      (1+ (vz N))))))
	(progn (v+ (v* (- 1.0 tb)
		       (vec3 1 1 1))
		   (v* tb
		       (vec3 0.5 0.7 1)))))))

(defun hit-sphere (center radius r)
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
				


(defun 1r-gradient-image ()
  (let ((iw 400)
	(ih 400))
    (render-pixels iw ih
      (let ((r (/ j (1- iw)))
	    (g (/ i (1- ih)))
	    (b 0.25))
	(format f "~D ~D ~D%"
	  (round (* 255.999 r))
	  (round (* 255.999 g))
	  (round (* 255.999 b)))))))

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
		   (1- iw)))
	     (v (/ j
		   (1- ih)))
	     (r (ray origin
		     (v- (v+ lower-left-corner ;; Scaling Camera's View to Viewport
			     (v* u horizontal)
			     (v* v vertical))
			 origin))))
	(if debug
	    (progn (print (float (/ j (1- ih))))))
	(write-color f (color-pixel r))))))
  
