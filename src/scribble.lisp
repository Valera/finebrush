(in-package :finebrush)

(defun widget-size (widget)
  (multiple-value-bind (w h) (gdk:drawable-get-size (widget-window widget))
    (list w h)))				    

(defun scribble ()
  (let (pixmap)
    (labels ((configure-event (widget event)
	       (declare (ignore event))
	       (bind:bind (((w h) (widget-size widget))
			   (window (widget-window widget))
			   (gc (graphics-context-new window)))
		 (setf pixmap (pixmap-new window w h -1))
		 (setf (graphics-context-rgb-fg-color gc)
		       (make-color :red 65535 :green 65535 :blue 65535))
		 (gdk::draw-rectangle pixmap gc t 0 0 w h))
	       t)
	     (expose-event (widget event)
	       (bind:bind ((rect (event-expose-area event))
			   (window (widget-window widget))
			   (gc (graphics-context-new window)))
		 (draw-drawable window gc pixmap
				(rectangle-x rect) (rectangle-y rect)
				(rectangle-x rect) (rectangle-y rect)
				(rectangle-width rect) (rectangle-height rect)))
	       nil)
	     (draw-brush (widget x y)
	       (bind:bind ((gc (graphics-context-new (widget-window widget)))
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
		     (setf x (round (event-motion-x event))
			   y (round (event-motion-y event))
			   state (event-motion-state event))
		     (bind:bind
			 (((:values _ x1 y1 state1)
			   (gdk-window-get-pointer (widget-window widget))))
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
			  #'(lambda (widget)
			      (iter (for i in '(:pointer-motion-mask
	 					:pointer-motion-hint-mask
						:leave-notify-mask
						:button-press-mask))
				    (pushnew i (gdk-window-events (widget-window widget))))))
	  (widget-show da)
	  (widget-show w))))))
