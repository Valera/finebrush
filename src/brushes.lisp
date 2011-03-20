(in-package :finebrush)

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

(defmethod draw-brush ((brush (eql :cairo-round)) pixmap x y pressure &key color)
  (with-gdk-context (ctx pixmap)
    (with-context (ctx)
      (let* ((x (round x))
	     (y (round y))
	     (r (round (* 10 pressure))))
	(set-source-color (color-red color) (color-green color) (color-blue color) pressure)
	(set-source-rgba 0.0 0.0 0.0 pressure)
	(arc x y r 0 (* 2 pi))
	(fill-path)
	(list (- x r) (- y r) (* 2 r) (* 2 r)))))
  (let ((x (round (- x (* 10 pressure))))
	(y (round (- y (* 10 pressure))))
	(w (round (* 20 pressure)))
	(h (round (* 20 pressure))))
    (list (- x 2) (- y 2) (+ 4 w) (+ 4 h))))

