(in-package :finebrush)

(defmacro with-gdk-cairo-context ((width height &optional context) gdk-drawable &body body)
  "Wraps 'body' with cairo context from gdk-drawable. If '&optional context' present,
it is bound to this context"
  (let ((ctx (if context
		 context
		 (gensym (string context)))))
    `(multiple-value-bind (,width ,height) (gdk:drawable-get-size ,gdk-drawable)
       (with-gdk-context (,ctx ,gdk-drawable)
	 (with-context (,ctx)
	   ,@body)))))	 

(defgeneric draw-brush (brush pixmap x y pressure &rest key-pairs)
  (:documentation "Generic function for drawing brush dub. Should be called by brush-stroke functions."))

(defmethod draw-brush ((brush (eql :simple-square)) pixmap x y pressure &key color)
  (let* ((gc (graphics-context-new pixmap))
	 (x (round (- x (* 10 pressure))))
	 (y (round (- y (* 10 pressure))))
	 (w (round (* 20 pressure)))
	 (h (round (* 20 pressure))))
    (setf (graphics-context-rgb-fg-color gc) color)
    (gdk::draw-rectangle pixmap gc t x y w h)
    (list x y w h)))

(defmethod draw-brush ((brush (eql :simple-round)) pixmap x y pressure &key color)
  (let* ((gc (graphics-context-new pixmap))
	 (x (round (- x (* 10 pressure))))
	 (y (round (- y (* 10 pressure))))
	 (w (round (* 20 pressure)))
	 (h (round (* 20 pressure))))
    (setf (graphics-context-rgb-fg-color gc) color)
    (gdk:draw-arc pixmap gc t x y w h 0 (* 64 360))
    (list x y w h)))

(defmethod draw-brush ((brush (eql :cairo-round-hard)) pixmap x y pressure &key color)
  (with-gdk-context (ctx pixmap)
    (with-context (ctx)
      (let* ((x (round x))
	     (y (round y))
	     (r (round (* 10 pressure))))
	(set-source-rgba (/ (color-red color) 65535d0)
			 (/ (color-green color) 65535d0)
			 (/ (color-blue color) 65535d0) pressure)
	(arc x y r 0 (* 2 pi))
	(fill-path)
	(list (- x r) (- y r) (* 2 r) (* 2 r))))))

(defmethod draw-brush ((brush (eql :cairo-round-soft)) pixmap x y pressure &key color alpha)
  (with-gdk-context (ctx pixmap)
    (with-context (ctx)
      (let* ((rad (* 10 pressure))
	     (r (/ (color-red color) 65535d0))
	     (g (/ (color-green color) 65535d0))
	     (b (/ (color-blue color) 65535d0)))
	(with-radial-pattern gradient
	    (x y (* 0.5 rad) x y (* rad))
	    `((0 (,r ,g ,b ,alpha))
	      (1 (,r ,g ,b 0)))
	  (arc x y rad 0 (* 2 pi))
	  (set-source gradient)
	  (fill-path)
	  (list (floor (- x rad)) (floor (- y rad))
		(ceiling (* 2 rad)) (ceiling (* 2 rad))))))))

	