(in-package :finebrush)

(defparameter *src-location* (asdf:component-pathname (asdf:find-system :finebrush)))

(cffi:defcfun gtk_rc_parse :void
  (filename (:pointer)))

(defun rc-parse (name)
  (cffi:with-foreign-string (foreign-name name)
    (gtk_rc_parse foreign-name)))

(defvar *zoom-sequence* '(1/4 1/2 2/3 1 4/3 3/2 2 4 8 16))

(defclass zoomer ()
  ((value :initform 1 :accessor value)
   (zoom-index :initform (position 1 *zoom-sequence* :test #'=) :accessor zoom-index)))

(defun zoom-in (zoomer)
  (let ((next-index (1+ (zoom-index zoomer))))
    (when (< next-index (length *zoom-sequence*))
      (incf (zoom-index zoomer))
      (setf (value zoomer) (elt *zoom-sequence* (zoom-index zoomer)))))
  (value zoomer))

(defun zoom-out (zoomer)
  (let ((next-index (1- (zoom-index zoomer))))
    (when (>= next-index 0)
      (decf (zoom-index zoomer))
      (setf (value zoomer) (elt *zoom-sequence* (zoom-index zoomer)))))
  (value zoomer))

;; FIXME: cleanup

(defparameter *canvas-margin* 400
  "Margin around painting in GtkFixed.")

(defun scribble-xinput ()
  (let (backing-pixmap
	canvas-pixbuf
	input-d
	brush-stroke
	hsv
	viewport
	spacing-adjustment
	alpha-adjustment
	vadjustment
	hadjustment
	fullscreen-p
	brush
	da0
	fixed
	drag-mode-p
	drag-p
	drag-x0
	drag-y0
	(zoomer (make-instance 'zoomer))
	(renew-backing-pixmap-p t)
	(builder (make-instance 'builder :from-file
				(namestring (merge-pathnames "ui/mainwin.glade" *src-location*)))))
    (labels ((bgo (object-name) (builder-get-object builder object-name))
	     (realize (widget)
	       (iter (for i in '(:pointer-motion-mask :pointer-motion-hint-mask
				 :leave-notify-mask :button-press-mask :button-release-mask))
		     (pushnew i (gdk-window-events (widget-window widget)))))
	     (configure-event (widget event)
	       (declare (ignore event))
	       (when renew-backing-pixmap-p
		 (setf renew-backing-pixmap-p nil)
		 (let* ((window (widget-window widget))
			(gc (graphics-context-new window)))
		   (multiple-value-bind (w h) (gdk:drawable-get-size (widget-window widget))
		     (print (list 'conf w h) *debug*)
		     (setf backing-pixmap (pixmap-new window w h -1))
		     (setf (graphics-context-rgb-fg-color gc)
			   (make-color :red 65535 :green 65535 :blue 65535))
		     (gdk::draw-rectangle backing-pixmap gc t 0 0 w h)))
		 t))
	     (expose-event (widget event)
	       (let* ((rect (event-expose-area event))
		      (window (widget-window widget))
		      (gc (graphics-context-new window)))
		 (draw-drawable window gc backing-pixmap
				(rectangle-x rect) (rectangle-y rect)
				(rectangle-x rect) (rectangle-y rect)
				(rectangle-width rect) (rectangle-height rect)))
	       nil)
	     (button-press-event (widget event)
	       (print (list 'abc (event-button-button event)) *debug*)
	       (if (or drag-p (= (event-button-button event) 2))
		   (setf drag-p t
			 drag-x0 (event-button-x event)
			 drag-y0 (event-button-y event))
		   (when (and backing-pixmap
			      (= (event-button-button event) 1))
		     (let ((pressure (event-get-axis event :pressure))
			   (source (gdk-device-source (event-button-device event))))
		       (setf brush-stroke (make-instance 'brush-stroke :pressure (if (eq :mouse source)
										     (clamp (or pressure 1) 0 1)
										     (clamp (or pressure 0) 0 1))
							 :x (event-button-x event) :y (event-button-y event)
							 :brush brush
							 :color (bind:bind (((:values h s v) (h-s-v-get-color hsv))
									    ((:values r g b) (h-s-v-to-r-g-b h s v))
									    (r1 (* 65535 r))
									    (g1 (* 65535 g))
									    (b1 (* 65535 b)))
								  (print (list r g b) *debug*)
								  (make-color :red r1 :green g1 :blue b1))
							 :spacing (adjustment-value spacing-adjustment)
							 :alpha (print (adjustment-value alpha-adjustment) *debug*)
							 :pixmap backing-pixmap :widget widget))))))
	     (button-release-event (widget event)
	       (declare (ignore widget event))
	       (setf brush-stroke nil
		     drag-p nil))
	     (motion-notify-event (widget event)
	       (print 'move *debug*)
	       (if drag-p
		   (progn
		     (print (- (event-motion-x event) drag-x0) *debug*)
		     (setf (adjustment-value hadjustment) (+ (adjustment-value hadjustment) (- drag-x0 (event-motion-x event)))
	 		   (adjustment-value vadjustment) (+ (adjustment-value vadjustment) (- drag-y0 (event-motion-y event)))))
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
		     (if (and backing-pixmap
			      (member :button1-mask state))
			 (when brush-stroke
			   (add-point-to-stroke brush-stroke x y (if (eq :mouse (gdk-device-source device))
								     (clamp (or pressure 1) 0 1)
								     (clamp (or pressure 0) 0 1)))
			   (draw-stroke brush-stroke))))))
	     (key-press-event (window event)
	       (if (eql (event-key-keyval event) 65480)
		   (if fullscreen-p
		       (progn
			 (gtk-window-unfullscreen window)
			 (setf fullscreen-p nil))
		       (progn
			 (gtk-window-fullscreen window)
			 (setf fullscreen-p t))))
	       (print event *debug*))
	     (initialize-model-and-combo-box (m c)
	       (store-add-column m "gchararray" #'identity)
	       (iter (for i in *brush-types*)
		     (store-add-item m (brush-name i)))
	       (let ((renderer (make-instance 'cell-renderer-text :text "A text")))
		 (cell-layout-pack-start c renderer :expand t)
		 (cell-layout-add-attribute c renderer "text" 0))
	       (setf (combo-box-model c) m)
	       (setf brush (first *brush-types*)
		     (combo-box-active c) 0))
	     (file-save (a)
	       (declare (ignore a))
	       (let ((d (make-instance 'file-chooser-dialog :action :save :title "Choose file name for saving")))
		 (dialog-add-button d "gtk-save" :accept)
		 (dialog-add-button d "gtk-cancel" :cancel)
		 (when (eq (dialog-run d) :accept)
		   (pixbuf-save (pixbuf-get-from-drawable nil backing-pixmap) (file-chooser-filename d) "png")
		   (format *debug* "saved to file ~A~%" (file-chooser-filename d)))
		 (object-destroy d)))
	     (change-canvas-size (w h gc &optional pixbuf)
	       (setf (widget-width-request da0) w
		     (widget-height-request da0) h)
	       (setf (widget-width-request fixed) (+ w (* 2 *canvas-margin*))
		     (widget-height-request fixed) (+ h (* 2 *canvas-margin*)))
	       (setf (fixed-child-x fixed da0) *canvas-margin*
		     (fixed-child-y fixed da0) *canvas-margin*)
	       (setf backing-pixmap (pixmap-new backing-pixmap w h -1))
	       (when pixbuf
		 (draw-pixbuf backing-pixmap gc pixbuf
			      0 0 0 0 -1 -1 :none 0 0)))
	     (file-load (a)
	       (declare (ignore a))
	       (let ((d (make-instance 'file-chooser-dialog :action :open :title "Choose file to open")))
		 (dialog-add-button d "gtk-open" :accept)
		 (dialog-add-button d "gtk-cancel" :cancel)
		 (when (eq (dialog-run d) :accept)
		   (let* ((gc (graphics-context-new backing-pixmap))
			  (pixbuf (pixbuf-new-from-file (file-chooser-filename d)))
			  (w (pixbuf-width pixbuf))
			  (h (pixbuf-height pixbuf)))
		     (change-canvas-size w h gc pixbuf)))
		 (object-destroy d)))
	     (create-input-dialog ()
	       (unless input-d
		 (setf input-d (make-instance 'input-dialog))
		 (connect-signal input-d "destroy"
				 #'(lambda (object)
				     (setf input-d nil)
				     (object-destroy object)))
		 (widget-show input-d))))
      (within-main-loop
;	(rc-parse "/usr/share/themes/Equinox Glass/gtk-2.0/gtkrc")
;	(rc-parse "/usr/share/themes/Redmond/gtk-2.0/gtkrc")
	(let ((w (bgo "window1"))
	      (da (bgo "drawingarea1" ))
	      (sw (bgo "scrolledwindow1"))
	      (v-box-2 (bgo "vbox2"))
	      (brush-combo (bgo "brush-combo-box"))
	      (brush-combo-model (make-instance 'array-list-store))
	      (h-s-v (make-instance 'h-s-v))
	      (new-painting-dialog (bgo "new-painting-dialog")))
	  (setf da0 da)
	  (setf fixed (bgo "fixed1"))
	  (initialize-model-and-combo-box brush-combo-model brush-combo)
	  (connect-signal brush-combo "changed" #'(lambda (arg) (declare (ignore arg))
							  (setf brush (nth (combo-box-active brush-combo) *brush-types*))))
	  (widget-modify-bg (bgo "viewport1") :normal (make-color :red 30000 :green 30000 :blue 30000))
	  (setf spacing-adjustment (bgo "spacing-adjustment"))
	  (setf (adjustment-value spacing-adjustment) 2d0)
	  (setf alpha-adjustment (bgo "brush-alpha-adjustment"))
	  (setf viewport (bgo "viewport1"))
	  (setf vadjustment (viewport-vadjustment viewport)
		hadjustment (viewport-hadjustment viewport))
	  (print (adjustment-lower (viewport-vadjustment viewport)) *debug*)
	  (box-pack-start v-box-2 h-s-v :expand nil :fill nil)
	  (setf hsv h-s-v)
	  (setf (widget-width-request h-s-v) 200)
	  (setf (widget-height-request h-s-v) 200)
	  (h-s-v-set-metrics h-s-v 180 20)
	  (iter (for device in (remove-if-not (lambda (x)
						(member (gdk-device-source x) '(:cursor :pen :eraser)))
					      (gdk-devices-list)))
		(setf (gdk-device-mode device) :screen))
	  (connect-signal sw "realize"
			  #'(lambda (sw0)
			      (setf (adjustment-value (scrolled-window-vadjustment sw0))
				    (+ (* 0.5 (- (adjustment-upper (scrolled-window-vadjustment sw0))
						 (adjustment-page-size (scrolled-window-vadjustment sw0)))))
							
				    (adjustment-value (scrolled-window-hadjustment sw0))
				    (+ (* 0.5 (- (adjustment-upper (scrolled-window-hadjustment sw0))
						 (adjustment-page-size (scrolled-window-hadjustment sw0))))))))
	  (connect-signal da "configure-event" #'configure-event)
	  (connect-signal da "expose-event" #'expose-event)
	  (connect-signal (bgo "new-painting-action") "activate" #'(lambda (action)
								     (declare (ignore action))
								     (widget-show new-painting-dialog)
								     (print (dialog-run new-painting-dialog) *debug*)
								     (widget-hide new-painting-dialog)))
	  (connect-signal (bgo "save-action") "activate" #'file-save)
	  (connect-signal (bgo "open-action") "activate" #'file-load)
	  (connect-signal (bgo "zoom-in-action") "activate"  #'(lambda (a) (declare (ignore a)) (print (zoom-in zoomer) *debug*)))
	  (connect-signal (bgo "zoom-out-action") "activate"  #'(lambda (a) (declare (ignore a)) (print (zoom-out zoomer) *debug*)))
	  (connect-signal (bgo "options-action") "activate" #'(lambda (a) (declare (ignore a)) (create-input-dialog)))
	  (connect-signal (bgo "quit-action") "activate" #'(lambda (action)
						      (declare (ignore action))
						      (object-destroy w)))
	  (connect-signal w "destroy" #'(lambda (w)
					  (declare (ignore w))
					  (leave-gtk-main)))
	  (connect-signal da "button_press_event" #'button-press-event)
	  (connect-signal da "button_release_event" #'button-release-event)
	  (connect-signal da "motion_notify_event" #'motion-notify-event)
	  (connect-signal w "key-press-event" #'key-press-event)
	  (connect-signal da "realize" #'realize)
	  (setf (widget-extension-events da) :cursor)
	  (widget-show da)
	  (widget-show w))))))
