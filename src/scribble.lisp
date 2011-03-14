(in-package :finebrush)

(defun widget-size (widget)
  (multiple-value-bind (w h) (gdk:drawable-get-size (widget-window widget))
    (list w h)))				    

(defun scribble ()
  (let (pixmap)
    (labels ((realize (widget)
	       (iter (for i in '(:pointer-motion-mask
				 :pointer-motion-hint-mask
				 :leave-notify-mask
				 :button-press-mask))
		     (pushnew i (gdk-window-events (widget-window widget)))))
	     (configure-event (widget event)
	       (declare (ignore event))
	       (let* ((window (widget-window widget))
		      (gc (graphics-context-new window)))
		 (multiple-value-bind (w h) (gdk:drawable-get-size (widget-window widget))
		   (setf pixmap (pixmap-new window w h -1))
		   (setf (graphics-context-rgb-fg-color gc)
			 (make-color :red 65535 :green 65535 :blue 65535))
		   (gdk::draw-rectangle pixmap gc t 0 0 w h))
		 t))
	     (expose-event (widget event)
	       (let* ((rect (event-expose-area event))
		      (window (widget-window widget))
		      (gc (graphics-context-new window)))
		 (draw-drawable window gc pixmap
				(rectangle-x rect) (rectangle-y rect)
				(rectangle-x rect) (rectangle-y rect)
				(rectangle-width rect) (rectangle-height rect)))
	       nil)
	     (draw-brush (widget x y)
	       (let ((gc (graphics-context-new (widget-window widget)))
		     (x (round x))
		     (y (round y)))
		 (gdk::draw-rectangle pixmap gc t
				      (- x 5) (- y 5) 10 10)
		 (widget-queue-draw-area widget (- x 5) (- y 5) 10 10)))
	     (button-press-event (widget event)
	       (when (and pixmap
			  (= (event-button-button event) 1))
		 (draw-brush widget (event-button-x event) (event-button-y event))))
	     (motion-notify-event (widget event)
	       (let (x y state)
		 (if (= (event-motion-is-hint event) 1)
		     (setf x (event-motion-x event)
			   y (event-motion-y event)
			   state (event-motion-state event))
		     (multiple-value-bind (retval x1 y1 state1)
			 (gdk-window-get-pointer (widget-window widget))
		       (declare (ignore retval))
		       (setf x x1 y y1 state state1)))
		 (if (and pixmap
			  (member :button1-mask state))
		     (draw-brush widget x y)))))
      (within-main-loop
	(let-ui (gtk-window
		 :var w
		 :title "Scribble"
		 :window-position :center
		 :default-width 500
		 :default-height 500
		 (v-box
		  :var v-box
		  (drawing-area
		   :var da
		   :default-width 200
		   :default-height 200) :expand t
		  (button
		   :label "quit"
		   :var button)
		  :expand nil))
	  (connect-signal da "configure-event"
			  #'configure-event)
	  (connect-signal da "expose-event"
			  #'expose-event)
	  (connect-signal button "clicked"
			  #'(lambda (args)
			      (declare (ignore args))
			      (object-destroy w)))
	  (connect-signal w "destroy"
			  #'(lambda (w)
			      (declare (ignore w))
			      (leave-gtk-main)))
	  (connect-signal da "button_press_event"
			  #'button-press-event)
	  (connect-signal da "motion_notify_event"
			  #'motion-notify-event)
	  (connect-signal da "realize"
			  #'realize)
	  (widget-show da)
	  (widget-show w))))))

(defun scribble-xinput ()
  (let (pixmap input-d)
    (labels ((realize (widget)
	       (iter (for i in '(:pointer-motion-mask
				 :pointer-motion-hint-mask
				 :leave-notify-mask
				 :button-press-mask))
		     (pushnew i (gdk-window-events (widget-window widget)))))
	     (configure-event (widget event)
	       (declare (ignore event))
	       (let* ((window (widget-window widget))
		      (gc (graphics-context-new window)))
		 (multiple-value-bind (w h) (gdk:drawable-get-size (widget-window widget))
		   (setf pixmap (pixmap-new window w h -1))
		   (setf (graphics-context-rgb-fg-color gc)
			 (make-color :red 65535 :green 65535 :blue 65535))
		   (gdk::draw-rectangle pixmap gc t 0 0 w h))
		 t))
	     (expose-event (widget event)
	       (let* ((rect (event-expose-area event))
		      (window (widget-window widget))
		      (gc (graphics-context-new window)))
		 (draw-drawable window gc pixmap
				(rectangle-x rect) (rectangle-y rect)
				(rectangle-x rect) (rectangle-y rect)
				(rectangle-width rect) (rectangle-height rect)))
	       nil)
	     (draw-brush (widget source x y pressure)
	       (print (list x y pressure) *debug*)
	       (flet ((greyscale-color (x)
			(print (list 'a x) *debug*)
			(make-color :red x :green x :blue x)))
		 (print source *debug*)
		 (let* (#+nil(gc (make-instance 'graphics-context
					  :rgb-fg-color (greyscale-color
							 (ecase source
							   (:mouse 15000)
							   (:pen 0)
							   (:eraser 65535)
							   (:cursor 45000)))))
			     (gc (graphics-context-new pixmap))
		       (pressure (clamp (or pressure 1) 0 1))
		       (x (round (- x (* 10 pressure))))
		       (y (round (- y (* 10 pressure))))
		       (w (round (* 20 pressure)))
		       (h (round (* 20 pressure))))
		   (setf (graphics-context-rgb-fg-color gc)
			 (greyscale-color
			  (ecase source
			    (:mouse 15000)
			    (:pen 0)
			    (:eraser 65535)
			    (:cursor 45000))))
		   (gdk::draw-rectangle pixmap gc t
					x y w h)
		   (widget-queue-draw-area widget x y w h))))
	     (button-press-event (widget event)
	       (when (and pixmap
			  (= (event-button-button event) 1))
		 (print (event-button-device event) *debug*)
		 (draw-brush widget (gdk-device-source (event-button-device event))
			     (event-button-x event) (event-button-y event)
			     (event-get-axis event :pressure))))
	     (motion-notify-event (widget event)
	       (print (event-motion-device event) *debug*)
	       (let ((device (event-motion-device event))
		     x y pressure state)
		 (print (event-motion-is-hint event) *debug*)
		 (if (= (event-motion-is-hint event) 1)
		     (setf x (event-get-axis event :x)
			   y (event-get-axis event :y)
			   pressure (event-get-axis event :pressure)
			   state (event-motion-state event))
		     (multiple-value-bind (retval x1 y1 state1)
			 (gdk-window-get-pointer (widget-window widget))
		       (declare (ignore retval))
		       (setf x (event-get-axis event :x) y y1 
			     pressure (event-get-axis event :pressure)
			     state state1)))
		 (iter (for i in '(:x :y :pressure :xtilt :ytilt))
		       (print (list i (event-get-axis event i)) *debug*))
			    
;		 (print (list 'x x 'pressure pressure) *debug*)
		 (if (and pixmap
			  (member :button1-mask state))
		     (draw-brush widget (gdk-device-source device)
				 x y pressure))))
	     (create-input-dialog ()
	       (unless input-d
		 (setf input-d (make-instance 'input-dialog))
;		 (connect-signal input-d "destroy"
;				 #'(lambda (&rest args) (declare (ignore args))))
;		 (connect-signal (dialog
		 (connect-signal input-d "destroy"
				 #'(lambda (object)
				     (setf input-d nil)
				     (object-destroy object)))

		 (widget-show input-d)))
)
      (within-main-loop
	(let-ui (gtk-window
		 :var w
		 :title "Scribble"
		 :window-position :center
		 :default-width 500
		 :default-height 500
		 (v-box
		  :var v-box
		  (drawing-area
		   :var da
		   :default-width 200
		   :default-height 200)
		  (button
		   :label "input dialog"
		   :var input-dialog-button)
		  :expand nil
		  (button
		   :label "quit"
		   :var button)
		  :expand nil))
	  (connect-signal da "configure-event"
			  #'configure-event)
	  (connect-signal da "expose-event"
			  #'expose-event)
	  (connect-signal input-dialog-button "clicked"
			  #'(lambda (args)
			      (declare (ignore args))
			      (create-input-dialog)))
	  (connect-signal button "clicked"
			  #'(lambda (args)
			      (declare (ignore args))
			      (object-destroy w)))
	  (connect-signal w "destroy"
			  #'(lambda (w)
			      (declare (ignore w))
			      (leave-gtk-main)))
	  (connect-signal da "button_press_event"
			  #'button-press-event)
	  (connect-signal da "motion_notify_event"
			  #'motion-notify-event)
	  (connect-signal da "realize"
			  #'realize)
	  (setf (widget-extension-events da) :cursor)
	  (widget-show da)
	  (widget-show w))))))
