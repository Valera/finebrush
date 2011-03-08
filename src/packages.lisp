(in-package :cl-user)

(defpackage :finebrush
  (:use :cl :alexandria :iterate :gtk :cl-cairo2 :cl-gtk2-cairo :gdk :gobject :gsll)
  (:shadowing-import-from :cl-cairo2 :scale :rotate :rectangle :pointer)
  (:shadowing-import-from :gsll #:range #:factorial #:median #:standard-deviation #:mean #:variance
			  #:multiply #:iterate)
  (:shadowing-import-from :iterate :sum))
