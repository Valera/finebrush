(in-package :finebrush)

(defparameter *src-location* (asdf:component-pathname (asdf:find-system :finebrush)))

(cffi:defcfun gtk_rc_parse :void
  (filename (:pointer)))

(defun rc-parse (name)
  (cffi:with-foreign-string (foreign-name name)
    (gtk_rc_parse foreign-name)))

;; FIXME: cleanup

(defun hsv-get-gtk-color (hsv)
  (bind:bind (((:values h s v) (h-s-v-get-color hsv))
	      ((:values r g b) (h-s-v-to-r-g-b h s v))
	      (r1 (* 65535 r))
	      (g1 (* 65535 g))
	      (b1 (* 65535 b)))
    (print (list r g b) *debug*)
    (make-color :red r1 :green g1 :blue b1)))

(defun scribble-xinput ()
  (let (input-d
	hsv
	spacing-adjustment
	alpha-adjustment
	vadjustment
	hadjustment
	fullscreen-p
	brush
	da0
	fixed
	(builder (make-instance 'builder :from-file
				(namestring (merge-pathnames "ui/mainwin.glade" *src-location*)))))
    (labels ((bgo (object-name) (builder-get-object builder object-name))
	     (realize (widget)
	       (iter (for i in '(:pointer-motion-mask :pointer-motion-hint-mask
				 :leave-notify-mask :button-press-mask :button-release-mask))
		     (pushnew i (gdk-window-events (widget-window widget)))))
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
	     (file-save-name ()
	       (let ((d (make-instance 'file-chooser-dialog :action :save :title "Choose file name for saving")))
		 (dialog-add-button d "gtk-save" :accept)
		 (dialog-add-button d "gtk-cancel" :cancel)
		 (prog1
		     (if (eq (dialog-run d) :accept)
			 (file-chooser-filename d)
			 nil)
		   (object-destroy d))))
	     (file-load-name ()
	       (let ((d (make-instance 'file-chooser-dialog :action :open :title "Choose file to open")))
		 (dialog-add-button d "gtk-open" :accept)
		 (dialog-add-button d "gtk-cancel" :cancel)
		 (prog1
		     (if (eq (dialog-run d) :accept)
			 (file-chooser-filename d)
			 nil)
		   (object-destroy d))))
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
	(let* ((w (bgo "window1"))
	       (da (bgo "drawingarea1" ))
	       (sw (bgo "scrolledwindow1"))
	       (v-box-2 (bgo "vbox2"))
	       (brush-combo (bgo "brush-combo-box"))
	       (brush-combo-model (make-instance 'array-list-store))
	       (h-s-v (make-instance 'h-s-v))
	       (new-painting-dialog (bgo "new-painting-dialog"))
	       (viewport (bgo "viewport1"))
	       (adjustment-painting-width (bgo "adjustment-painting-width"))
	       (adjustment-painting-height (bgo "adjustment-painting-height"))
	       (color-button (bgo "colorbutton1"))
	       (canvas (make-instance 'canvas :width 400 :height 400 :display-widget da :fixed-widget (bgo "fixed1")
				      :hadjustment (viewport-hadjustment viewport) 
				      :vadjustment (viewport-vadjustment viewport) :scrolled-window sw
				      :fg-color (hsv-get-gtk-color h-s-v) :brush :cairo-round-hard
				      :bg-color (make-color :red 65535 :green 65535 :blue 65535))))
	  (setf da0 da)
	  (setf fixed (bgo "fixed1"))
	  (initialize-model-and-combo-box brush-combo-model brush-combo)
	  (connect-signal brush-combo "changed" #'(lambda (arg) (declare (ignore arg))
							  (setf (brush canvas) (nth (combo-box-active brush-combo) *brush-types*))))
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
	  (connect-signal (bgo "new-painting-action") "activate" 
			  (lambda (action)
			    (declare (ignore action))
			    (widget-show new-painting-dialog)
			    (when (eq (dialog-run new-painting-dialog) :ok)
			      (when canvas
				(disconnect-signals canvas))
			      (setf canvas
				    (make-instance 'canvas :width (ceiling (adjustment-value adjustment-painting-width))
						   :height (ceiling (adjustment-value adjustment-painting-height))
						   :display-widget da :fixed-widget (bgo "fixed1")
						   :hadjustment (viewport-hadjustment viewport) 
						   :vadjustment (viewport-vadjustment viewport) :scrolled-window sw
						   :fg-color (hsv-get-gtk-color hsv) :brush :cairo-round-hard
						   :bg-color (color-button-color color-button))))
			    (widget-hide new-painting-dialog)))
	  (connect-signal (bgo "open-action") "activate"
			  (lambda (a) (declare (ignore a))
				  (when-let (fname (file-load-name))
				    (disconnect-signals canvas)
				    (setf canvas (make-instance 'canvas :filename fname
								:display-widget da :fixed-widget (bgo "fixed1")
								:hadjustment (viewport-hadjustment viewport)
								:vadjustment (viewport-vadjustment viewport) :scrolled-window sw
								:fg-color (make-color :green 65535) :brush :cairo-round-hard
								:bg-color (make-color :red 65535 :green 65535 :blue 65535))))))
	  (connect-signal (bgo "save-action") "activate" 
			  (lambda (a) (declare (ignore a))
				  (when-let (fname (file-save-name))
				    (save-canvas canvas fname))))
	  (connect-signal (bgo "zoom-in-action") "activate"  #'(lambda (a)
								 (declare (ignore a))
								 (print (zoom-in canvas) *debug*)))
	  (connect-signal (bgo "zoom-out-action") "activate"  #'(lambda (a)
								  (declare (ignore a))
								  (print (zoom-out canvas) *debug*)))
	  (connect-signal (bgo "options-action") "activate" #'(lambda (a) (declare (ignore a)) (create-input-dialog)))
	  (connect-signal (bgo "quit-action") "activate" #'(lambda (action)
							     (declare (ignore action))
							     (object-destroy w)))
	  (connect-signal w "destroy" #'(lambda (w)
					  (declare (ignore w))
					  (leave-gtk-main)))
	  (connect-signal hsv "changed" (lambda (hsv) (setf (fg-color canvas) (hsv-get-gtk-color hsv))))
	  (connect-signal w "key-press-event" #'key-press-event)
	  (connect-signal da "realize" #'realize)
	  (setf (widget-extension-events da) :cursor)
	  (widget-show da)
	  (widget-show w))))))
