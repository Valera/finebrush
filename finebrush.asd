;;; finebrush.asd
 
(defpackage :finebrush-system
  (:use :cl :asdf))

(in-package :finebrush-system)
 
(defsystem :finebrush
  :serial t
  :depends-on (:split-sequence :alexandria :iterate :cl-gtk2-gtk
			:cl-cairo2 :cl-gtk2-cairo :metabang-bind
			:gsll)
  :components ((:module "src"
			:serial t
			:components ((:file "packages")
				     (:file "curve-widget")
				     (:file "brush-stroke")
				     (:file "scribble")))))
