;; curve-widget.lisp

(in-package :finebrush)

;;; FIXME: recactor it :(
;;; FIXME: separate class for Curwe Widget.

(defun widget-size (widget)
  (multiple-value-bind (w h) (gdk:drawable-get-size (widget-window widget))
    (list w h)))

(defparameter *debug* *standard-output*)

(defun grid-from-vector (vector)
  (iter (with grid = (grid:make-foreign-array 'double-float :dimensions (length vector)))
	(for x in-vector vector)
	(for i upfrom 0)
	(setf (grid:gref grid i) (coerce x 'double-float))
	(finally (return grid))))

(defun draw-spline (w h x-vector y-vector highlight-index &optional (context *context*))
  (with-context (context)
    (scale w h)
    (set-source-rgb 0.5 0.5 0.5)
    (set-line-width 0.01)
    (set-line-cap :round)
    (set-line-join :round)
    ;; Draw spline
    (iter (with acc = (make-acceleration))
	  (with x-grid = (grid-from-vector x-vector))
	  (with y-grid = (grid-from-vector y-vector))
	  (with spline = (make-spline +cubic-spline-interpolation+ x-grid y-grid))
	  (for x from (aref x-vector 0) to (last-elt x-vector) by 0.01d0)
	  (for y = (clamp (gsll:evaluate spline (coerce x 'double-float) :acceleration acc :xa x-grid :ya y-grid)
			  0 1))
	  (if (first-time-p)
	      (move-to x y)
	      (line-to x y)))
    (stroke)
    (set-source-rgb 0.0 0.5 0.5)
    (iter (for x in-vector x-vector)
	  (for y in-vector y-vector)
	  (arc x y 0.05 0 (* 2 pi))
	  (stroke))
    (when highlight-index
      (print (list 'ind highlight-index))
      (set-source-rgb 0.6 0.7 0.7)
      (arc (aref x-vector highlight-index) (aref y-vector highlight-index) 0.05 0 (* 2 pi))
      (stroke))))

(defun hypot (x y)
  (sqrt (+ (* x x) (* y y))))

(defun insert-to-vector (vector ind val)
  (let ((vector (adjust-array vector (+ 1 (length vector)))))
    (print (list vector (1+ ind) (length vector)) *debug*)
    (print (list vector ind (length vector)) *debug*)
    (setf (subseq vector (1+ ind) (length vector)) (subseq vector ind (length vector))
	  (aref vector ind) val)
    vector))

(defun insert-point (x  y  x-vector y-vector)
  (iter (for xi in-vector x-vector)
	(for i upfrom 0)
	(when (> xi x)
	  (print (list 'hi i x) *debug*)
	  (print x-vector *debug*)
	  (when (and (> i 1)
		     (= x (aref x-vector (1- i))))
	    (return))
	  (insert-to-vector x-vector i x)
	  (print x-vector *debug*)
	  (insert-to-vector y-vector i y)
	  (return))))

(defun remove-at-pos (pos sequence)
  (let ((n pos))
    (remove-if #'(lambda (x) (declare (ignore x)) (= (decf n) 0))
	       sequence)))

(defun move-point (new-x new-y index x-vector y-vector)
  (block nil
    (let ((found-index (position new-x x-vector)))
      (when (and found-index (/= found-index index))
	(setf (aref y-vector index) new-y)
	(return index)))
    (let ((insert-index (position-if #'(lambda (x) (> x new-x)) x-vector)))
      (when (= index insert-index)
	(print "Bingo" *debug*)
	(setf (aref x-vector index) new-x
	      (aref y-vector index) new-y)
	(return index))
      (setf (subseq x-vector 0) (insert-to-vector (remove-at-pos (1+ index) x-vector)
						  (if (< index insert-index)
						      (1- insert-index)
						      insert-index)
						  new-x)
	    (subseq y-vector 0) (insert-to-vector (remove-at-pos (1+ index) y-vector)
						  (if (< index insert-index)
						      (1- insert-index)
						      insert-index)
						  new-y))
      (return 
	(if (< index insert-index)
	    (1- insert-index)
	    insert-index)))))

(defun run-curve-widget ()
  (within-main-loop
    (let ((selected-point-index)
	  (w0)
	  (h0)
	  (x-vector (copy-array #(0d0 0.2d0 0.4d0 1.0d0) :adjustable t))
	  (y-vector (copy-array #(0d0 0.3d0 0.3d0 1.0d0) :adjustable t)))
      (let-ui
	  (gtk-window
	   :var w
	   :title "Gtk+ demo for Lisp"
	   :window-position :center
	   :default-width 500
	   :default-height 500
	   (drawing-area
	    :var da))
	(gobject:connect-signal da "configure-event" (lambda (widget event)
						       (declare (ignore event))
						       (widget-queue-draw widget)))
	(gobject:connect-signal da "expose-event"
				(lambda (widget event)
				  (declare (ignore event))
				  (multiple-value-bind (w h) (gdk:drawable-get-size (widget-window widget))
				    (with-gdk-context (ctx (widget-window widget))
				      (with-context (ctx)
					(setf w0 w)
					(setf h0 h)
					(funcall #'draw-spline w h x-vector y-vector selected-point-index)
					nil)))))
	(connect-signal da "realize"
			#'(lambda (widget)
					;(declare (ignore widget))
			    (pushnew :pointer-motion-mask (gdk-window-events (widget-window widget)))
			    (pushnew :button-press-mask (gdk-window-events (widget-window widget)))
			    (pushnew :button-release-mask (gdk-window-events (widget-window widget)))))
	(connect-signal da "button_press_event"
			#'(lambda (widget event)
			    (let* ((x0 (/ (event-button-x event) w0))
				   (y0 (/ (event-button-y event) h0)))
			      (if (eql :2button-press (event-button-type event))
				  (progn (insert-point x0 y0 x-vector y-vector)
					 (widget-queue-draw widget))
				  (iter (for x in-vector x-vector)
					(for y in-vector y-vector)
					(for i upfrom 0)
					(when (< (hypot (- x x0) (- y y0)) 0.05)
					  (setf selected-point-index i)
					  (widget-queue-draw widget)
					  (return i)))))))
	(connect-signal da "button-release-event"
			#'(lambda (widget event)
			    (declare (ignore event))
			    (setf selected-point-index nil)
			    (widget-queue-draw widget)))
	(connect-signal da "motion-notify-event"
			#'(lambda (widget event &rest args)
			    (declare (ignore args))
			    (when selected-point-index
			      (setf selected-point-index
				    (move-point (/ (event-motion-x event) w0) (/ (event-motion-y event) h0)
						selected-point-index x-vector y-vector))
			      (widget-queue-draw widget))))
	(widget-show w)))))
