(in-package :finebrush)

(defclass brush-stroke ()
  ((l0 :initform 0d0)
   (l :initform 0d0)
   (dub-index :initform 0)
   (point-list :initform '())
   (widget :initarg :widget)
   (pixmap :initarg :pixmap)
   (spacing :initarg :spacing)
   (color :initarg :color)
   (brush :initarg :brush)))

(defmethod initialize-instance :after ((instance brush-stroke) &rest initargs &key x y pressure &allow-other-keys)
  (declare (ignore initargs))
  (setf *prev* (list x y))
  (with-slots (point-list brush pixmap widget color)
      instance
    (push (list x y pressure) point-list)
    (bind:bind (((x y w h) (draw-brush brush pixmap x y pressure :color color)))
      (widget-queue-draw-area widget x y w h))))

(defun add-point-to-stroke (brush-stroke x y pressure)
  (with-slots (point-list brush pixmap widget color)
      brush-stroke
    (push (list x y pressure) point-list)
    (print point-list *debug*)
))

(defun interp (x0 x1 param)
  "Interposates value beetween x0 and x1. Param is number from 0 to 1."
  (+ x0 (* (- x1 x0) param)))

(defun draw-stroke (brush-stroke)
  (with-slots (point-list brush pixmap widget color dub-index l l0 spacing)
      brush-stroke
    (iter (for i from 0 below (1- (length point-list)))
	  (for (x1 y1 pressure1) = (nth i point-list))
	  (for (x0 y0 pressure0) = (nth (1+ i) point-list))
	  (for len = (hypot (- x0 x1) (- y0 y1)))
	  (iter (while (< l (+ l0 len)))
		(incf dub-index)
		(when (= dub-index 1) ;; Do not double first dub, it is already placed at button press event.
		  (incf l spacing)
		  (next-iteration))
		(for interp-param = (/ (- l l0) len))
		(bind:bind (((x y w h) (draw-brush brush pixmap (interp x0 x1 interp-param)
						   (interp y0 y1 interp-param) (interp pressure0 pressure1 interp-param)
						   :color color :stroke-length l)))
		  (widget-queue-draw-area widget x y w h))
;		(print (list 'data y0 y1 len l l0) *debug*)
;		(print (list 'hypot (hypot (- (+ x0 (* (/ (- x1 x0) len) (- l l0))) (first *prev*))
;					   (- (+ y0 (* (/ (- y1 y0) len) (- l l0))) (second *prev*))))
;		       *debug*)
;		(setf *prev* (list (+ x0 (* (/ (- x1 x0) len) (- l l0)))
;				   (+ y0 (* (/ (- y1 y0) len) (- l l0)))))
#|
		(print (list 'now (+ x0 (* (/ (- x1 x0) len) (- l l0)))  (+ y0 (* (/ (- y1 y0) len) (- l l0)))) *debug*)
		(print (list 'bef *prev*) *debug*)

		
		
|#		
		(incf l spacing))
	    (incf l0 len))
      (setf (cdr point-list) nil)
;      (print (list 'a (- l l0)) *debug*)
;      (print points *debug*)
))

(defvar *prev* 0)
#|
(defun move-brush-lambda (x0 y0 pixmap draw-fun pressure source spacing)
  (setf *prev* (list x0 y0))
  (funcall draw-fun pixmap x0 y0 pressure 0)
  (let ((spacing (coerce spacing 'double-float))
	(l 0d0)
	(l0 0d0)
	(dub-index 0)
	(points ())
	)
  (lambda (x y pressure)
    (push (list x y) points)
    (iter (for i from 0 below (1- (length points)))
	  (for (x1 y1) = (nth i points))
	  (for (x0 y0) = (nth (1+ i) points))
	  (for len = (hypot (- x0 x1) (- y0 y1)))
	  (iter (while (< l (+ l0 len)))
		(incf dub-index)
		(when (= dub-index 1) ;; Do not double first dub, it is already placed at button press event.
		  (incf l spacing)
		  (next-iteration))
		(funcall draw-fun pixmap
			 (+ x0 (* (/ (- x1 x0) len) (- l l0)))
			 (+ y0 (* (/ (- y1 y0) len) (- l l0)))
			 pressure l)
		(print (list 'data y0 y1 len l l0) *debug*)
		(print (list 'hypot (hypot (- (+ x0 (* (/ (- x1 x0) len) (- l l0))) (first *prev*))
					   (- (+ y0 (* (/ (- y1 y0) len) (- l l0))) (second *prev*))))
		       *debug*)
		(print (list 'now (+ x0 (* (/ (- x1 x0) len) (- l l0)))  (+ y0 (* (/ (- y1 y0) len) (- l l0)))) *debug*)
		(print (list 'bef *prev*) *debug*)

		(setf *prev* (list (+ x0 (* (/ (- x1 x0) len) (- l l0)))
				   (+ y0 (* (/ (- y1 y0) len) (- l l0)))))
		(incf l spacing))
    	  (incf l0 len))
    (setf (cdr points) nil)
    (print (list 'a (- l l0)) *debug*)
    (print points *debug*)
)))

|#