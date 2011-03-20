(in-package :finebrush)

(defun scribble ()
  (let (pixmap)
    (labels ((realize (widget)
	       (iter (for i in '(:pointer-motion-mask :pointer-motion-hint-mask
				 :leave-notify-mask :button-press-mask))
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
		   :default-height 200)
		  (button
		   :label "quit"
		   :var button)
		  :expand nil))
	  (connect-signal da "configure-event" #'configure-event)
	  (connect-signal da "expose-event" #'expose-event)
	  (connect-signal button "clicked" #'(lambda (args)
					       (declare (ignore args))
					       (object-destroy w)))
	  (connect-signal w "destroy" #'(lambda (w)
					  (declare (ignore w))
					  (leave-gtk-main)))
	  (connect-signal da "button_press_event" #'button-press-event)
	  (connect-signal da "motion_notify_event" #'motion-notify-event)
	  (connect-signal da "realize" #'realize)
	  (widget-show da)
	  (widget-show w))))))

(defun scribble-xinput ()
  (let (pixmap input-d brush-stroke)
    (labels ((realize (widget)
	       (iter (for i in '(:pointer-motion-mask :pointer-motion-hint-mask
				 :leave-notify-mask :button-press-mask :button-release-mask))
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
	       (flet ((greyscale-color (x)
			(make-color :red x :green x :blue x)))
		 (let* ((gc (graphics-context-new pixmap))
			(pressure (if (eq :mouse source)
				      (clamp (or pressure 1) 0 1)
				      (clamp (or pressure 0) 0 1)))
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
		 (let ((pressure (event-get-axis event :pressure))
		       (source (gdk-device-source (event-button-device event))))
		   (setf brush-stroke (make-instance 'brush-stroke :pressure (if (eq :mouse source)
										 (clamp (or pressure 1) 0 1)
										 (clamp (or pressure 0) 0 1))
						     :x (event-button-x event) :y (event-button-y event)
						     :brush :cairo-round
						     :color (flet ((greyscale-color (x)
								     (make-color :red x :green x :blue x)))
							      (greyscale-color
							       (ecase source
								 (:mouse 15000)
								 (:pen 0)
								 (:eraser 65535)
								 (:cursor 45000))))
						     :spacing 25 :pixmap pixmap :widget widget)))
		   
#+nil
		 (setf stroke-fun (move-brush-lambda (event-button-x event) (event-button-y event)
						     pixmap #'(lambda (pixmap x y pressure l)
								(declare (ignore pixmap l))
								(draw-brush widget (gdk-device-source (event-button-device event))
									    x y pressure))
						     (event-get-axis event :pressure)
						     (gdk-device-source (event-button-device event)) 25))
#+nil		 (draw-brush widget (gdk-device-source (event-button-device event))
			     (event-button-x event) (event-button-y event)
			     (event-get-axis event :pressure))))
	     (button-release-event (widget event)
	       (setf brush-stroke nil))
	     (motion-notify-event (widget event)
	       (let ((device (event-motion-device event))
		     x y pressure state)
		 (if (= (event-motion-is-hint event) 1)
		     (setf x (event-get-axis event :x)
			   y (event-get-axis event :y)
			   pressure (event-get-axis event :pressure)
			   state (event-motion-state event))
		     (multiple-value-bind (retval x1 y1 state1)
			 (gdk-window-get-pointer (widget-window widget))
		       (declare (ignore retval))
		       (setf x x1 y y1 
			     pressure (event-get-axis event :pressure)
			     state state1)))
		 (if (and pixmap
			  (member :button1-mask state))
		     (when brush-stroke
		       (add-point-to-stroke brush-stroke x y (if (eq :mouse (gdk-device-source device))
								 (clamp (or pressure 1) 0 1)
								 (clamp (or pressure 0) 0 1)))
		       (draw-stroke brush-stroke)
#+nil		       (funcall stroke-fun x y pressure)
#+nil		     (draw-brush widget (gdk-device-source device)
				 x y pressure)))))
	     (create-input-dialog ()
	       (unless input-d
		 (setf input-d (make-instance 'input-dialog))
		 (connect-signal input-d "destroy"
				 #'(lambda (object)
				     (setf input-d nil)
				     (object-destroy object)))
		 (widget-show input-d))))
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
	  (connect-signal da "configure-event" #'configure-event)
	  (connect-signal da "expose-event" #'expose-event)
	  (connect-signal input-dialog-button "clicked" #'(lambda (args)
							    (declare (ignore args))
							    (create-input-dialog)))
	  (connect-signal button "clicked" #'(lambda (args)
					       (declare (ignore args))
					       (object-destroy w)))
	  (connect-signal w "destroy" #'(lambda (w)
					  (declare (ignore w))
					  (leave-gtk-main)))
	  (connect-signal da "button_press_event" #'button-press-event)
	  (connect-signal da "button_release_event" #'button-release-event)
	  (connect-signal da "motion_notify_event" #'motion-notify-event)
	  (connect-signal da "realize" #'realize)
	  (setf (widget-extension-events da) :cursor)
	  (widget-show da)
	  (widget-show w))))))
