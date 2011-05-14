(in-package :finebrush)

(defparameter *zoom-sequence* '(1/4 1/2 2/3 1 3/2 2 4 8 16))
(defparameter *canvas-margin* 400
  "Margin around painting in GtkFixed.")

(defclass zoomer ()
  ((zoom-factor :initform 1 :accessor zoom-factor)
   (zoom-index :initform (position 1 *zoom-sequence* :test #'=) :accessor zoom-index))
  (:documentation "Mixin class for zooming functionality"))

(defgeneric zoom-in (zoomer))
(defmethod zoom-in ((zoomer zoomer))
  (let ((next-index (1+ (zoom-index zoomer))))
    (when (< next-index (length *zoom-sequence*))
      (incf (zoom-index zoomer))
      (setf (zoom-factor zoomer) (float (elt *zoom-sequence* (zoom-index zoomer)) 1d0))))
  (zoom-factor zoomer))

(defgeneric zoom-out (zoomer))
(defmethod  zoom-out ((zoomer zoomer))
  (let ((next-index (1- (zoom-index zoomer))))
    (when (>= next-index 0)
      (decf (zoom-index zoomer))
      (setf (zoom-factor zoomer) (float (elt *zoom-sequence* (zoom-index zoomer)) 1d0))))
  (zoom-factor zoomer))



(defclass canvas (zoomer)
  ((bg-color :accessor bg-color :initarg :bg-color)
   (fg-color :accessor fg-color :initarg :fg-color)
   (baselayer :accessor baselayer :initform nil)
   (backing-pixmap :accessor backing-pixmap :initform nil)
   (display-widget :accessor display-widget :initarg :display-widget)
   (fixed-widget :initarg :fixed-widget)
   (scrolled-window :initarg :scrolled-window)
   (hadjustment :initarg :hadjustment)
   (vadjustment :initarg :vadjustment)
   (width :accessor width :initarg :width)
   (height :accessor height :initarg :height)
   (filename :accessor filename :initarg :filename :initform nil)
   (drag-p :initform nil)
   drag-x0
   drag-y0
   brush-stroke
   spacing-adjustmen ;; FIXME: brush preferences object
   alpha-adjustment
   (handler-id-list :accessor handler-id-list :initform nil)
   (brush :accessor brush :initarg :brush)
   (scaled-width :accessor scaled-width)
   (scaled-height :accessor scaled-height)))

(defun update-canvas-scale (canvas)
  (with-slots (zoom-factor width height scaled-width scaled-height)
      canvas
    (setf scaled-width (floor (* width zoom-factor))
	  scaled-height (floor (* height zoom-factor)))))

(defun update-backing-pixmap (canvas)
  (bind (((:slots zoom-factor scaled-width scaled-height backing-pixmap
		  baselayer display-widget fixed-widget)
	  canvas)
	 (window (widget-window display-widget))
	 (gc (graphics-context-new window)))
    (print (list 'ubp window) *debug*)
    (setf backing-pixmap (pixmap-new window scaled-width scaled-height -1))
 ;   (pixbuf-scale baselayer backing-pixmap 0 0 scaled-width scaled-height 0 0
;		  (float zoom-factor) (float zoom-factor) :bilinear)
    (setf (graphics-context-rgb-fg-color gc)
	  (make-color :red 65535 :green 65535 :blue 65535))
;    (gdk::draw-rectangle backing-pixmap gc t 0 0 scaled-width scaled-height)
    (draw-drawable backing-pixmap gc baselayer 0 0 0 0 scaled-height scaled-height)
    (setf (widget-width-request display-widget) scaled-width
	  (widget-height-request display-widget) scaled-height)
    (setf (widget-width-request fixed-widget) (+ scaled-width (* 2 *canvas-margin*))
	  (widget-height-request fixed-widget) (+ scaled-height (* 2 *canvas-margin*)))
    (setf (fixed-child-x fixed-widget display-widget) *canvas-margin*
	  (fixed-child-y fixed-widget display-widget) *canvas-margin*)
    (widget-queue-draw fixed-widget)))

(defmethod zoom-in :after ((canvas canvas))
  (update-canvas-scale canvas)
  (update-backing-pixmap canvas))

(defmethod zoom-out :after ((canvas canvas))
  (update-canvas-scale canvas)
  (update-backing-pixmap canvas))

(defun canvas-expose (canvas widget event)
  (print canvas *debug*)
  (bind (((:slots zoom-factor baselayer backing-pixmap width height) canvas)
	 (rect (event-expose-area event))
	 (window (widget-window widget))
	 (gc (graphics-context-new window)))
    (print (list 'bp backing-pixmap) *debug*)
    (when (and baselayer backing-pixmap)
      (with-gdk-context (ctx backing-pixmap)
	(with-context (ctx)
	  (print 1 *debug*)
	  (rectangle (rectangle-x rect) (rectangle-y rect)
		     (rectangle-width rect) (rectangle-height rect))
	  (clip)
	  (save)
	  (print 2 *debug*)
	  (scale (float zoom-factor) (float zoom-factor))
	  (gdk-cairo-set-source-pixmap baselayer 0d0 0d0)
	  (rectangle (/ (rectangle-x rect) zoom-factor)
		     (/ (rectangle-y rect) zoom-factor)
		     (/ (rectangle-width rect) zoom-factor)
		     (/ (rectangle-height rect) zoom-factor))
	  (print 3 *debug*)
	  (clip)
	  (paint)
	  (restore)))
      (draw-drawable window gc backing-pixmap
		     (rectangle-x rect) (rectangle-y rect)
		     (rectangle-x rect) (rectangle-y rect)
		     (rectangle-width rect) (rectangle-height rect))))
  nil)

(defun canvas-button-press (canvas widget event)
  (bind (((:slots drag-p drag-x0 drag-y0 backing-pixmap brush-stroke zoom-factor
		  spacing-adjustment alpha-adjustment fg-color brush baselayer)
	  canvas)
	 (x (/ (event-button-x event) zoom-factor))
	 (y (/ (event-button-y event) zoom-factor)))
    (if (or drag-p (= (event-button-button event) 2))
	;; start dragging
	(setf drag-p t
	      drag-x0 (event-button-x event)
	      drag-y0 (event-button-y event))
	;; start brush stroke
	(when (and backing-pixmap
		   (= (event-button-button event) 1))
	  (let ((pressure (event-get-axis event :pressure))
		(source (gdk-device-source (event-button-device event))))
	    (setf brush-stroke (make-instance 'brush-stroke :pressure (if (eq :mouse source)
									  (clamp (or pressure 1) 0 1)
									  (clamp (or pressure 0) 0 1))
					      :x x :y y
					      :brush brush
					      :color fg-color
					      :spacing 2; (adjustment-value spacing-adjustment)
					      :alpha 0.8 ;(print (adjustment-value alpha-adjustment) *debug*)
					      :pixmap baselayer :widget widget)))))))

(defun canvas-button-release (canvas widget event)
  (declare (ignore widget event))
  (bind (((:slots brush-stroke drag-p) canvas))
    (setf brush-stroke nil
	  drag-p nil)))

(defun canvas-motion-notify (canvas widget event)
  (bind (((:slots drag-p drag-x0 drag-y0 hadjustment vadjustment backing-pixmap
		  brush-stroke display-widget zoom-factor)
	  canvas))
    (if drag-p
	;; Drag painting.
	(progn
	  (print (- (event-motion-x event) drag-x0) *debug*)
	  (setf (adjustment-value hadjustment) (+ (adjustment-value hadjustment) (- drag-x0 (event-motion-x event)))
		(adjustment-value vadjustment) (+ (adjustment-value vadjustment) (- drag-y0 (event-motion-y event)))))
	;; Continue brush stroke.
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
	  (setf x (/ x zoom-factor)
		y (/ y zoom-factor))
	  (if (and backing-pixmap
		   (member :button1-mask state))
	      (when brush-stroke
		(add-point-to-stroke brush-stroke x y (if (eq :mouse (gdk-device-source device))
							  (clamp (or pressure 1) 0 1)
							  (clamp (or pressure 0) 0 1)))
		(flet ((zoom (x) (* x zoom-factor)))
		  (iter (for (x y w h) in (draw-stroke brush-stroke))
			(widget-queue-draw-area display-widget 
						(floor (zoom x)) (floor (zoom y))
						(ceiling (zoom w)) (ceiling (zoom h)))))))))))

(defmethod initialize-instance :after ((canvas canvas) &key  &allow-other-keys)
  (bind (((:slots (da display-widget) handler-id-list) canvas))
    (setf handler-id-list
	  (list
	   (prog1 (connect-signal da "configure-event" #'(lambda (w e) (declare (ignore w e)) (canvas-configure canvas)))
	     (setf (widget-width-request da) 10)
#+nil	     (emit-signal da "configure-event"))
	   (connect-signal da "expose-event" #'(lambda (w e) (canvas-expose canvas w e)))
	   (connect-signal da "button_press_event" #'(lambda (w e) (canvas-button-press canvas w e)))
	   (connect-signal da "button_release_event" #'(lambda (w e) (canvas-button-release canvas w e)))
	   (connect-signal da "motion_notify_event" #'(lambda (w e) (canvas-motion-notify canvas w e)))))))

(defun disconnect-signals (canvas)
  (iter (for id in (handler-id-list canvas))
	(disconnect-signal (display-widget canvas) id)))

(defun canvas-configure (canvas)
  (bind (((:slots (w width) (h height) display-widget bg-color
		  (sw0 scrolled-window) filename baselayer) canvas))
    (print (list 'cc 'baselayer baselayer) *debug*)
    (unless baselayer
      (bind ((pixbuf (and filename (pixbuf-new-from-file filename))))
	(when pixbuf
	  (setf w (pixbuf-width pixbuf)
		h (pixbuf-height pixbuf)))
	(setf (widget-width-request display-widget) w
	      (widget-height-request display-widget) h)
	(setf baselayer (pixmap-new (widget-window display-widget) w h -1))
	(let ((gc (graphics-context-new (baselayer canvas))))
	  (setf (graphics-context-rgb-fg-color gc) bg-color)
	  (if pixbuf
	      (draw-pixbuf baselayer gc pixbuf 0 0 0 0 w h :none 0 0)
	      (gdk::draw-rectangle baselayer gc t 0 0 w h)))
	(update-canvas-scale canvas)
	(update-backing-pixmap canvas)))
      (setf (adjustment-value (scrolled-window-vadjustment sw0))
	    (+ (* 0.5 (- (adjustment-upper (scrolled-window-vadjustment sw0))
			 (adjustment-page-size (scrolled-window-vadjustment sw0)))))
							
	    (adjustment-value (scrolled-window-hadjustment sw0))
	    (+ (* 0.5 (- (adjustment-upper (scrolled-window-hadjustment sw0))
			 (adjustment-page-size (scrolled-window-hadjustment sw0)))))))
  t)

(defun save-canvas (canvas filename)
  (pixbuf-save (pixbuf-get-from-drawable nil (baselayer canvas)) filename "png"))
