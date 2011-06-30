(in-package :finebrush)

(defun points-to-file (points)
  (with-png-file ("/home/vfedotov/temp/temp.png" :argb32 100 100)
    (rectangle 0 0 100 100)
    (set-source-rgba 0 0 0 1)
    (fill-path)
    (let* ((max (reduce (lambda (x y) (max (abs x) (abs y)))
			points :key (lambda (l) (max (abs (first l)) (abs (second l))))))
	   (factor (/ 45 max)))
      (translate 50 50)
      (print (/ factor 50))
      (scale factor factor)
      (set-source-rgba 1 0 0 1)
      (iter (for (x y) in points)
	    (print (list x y))
	    (arc x y  (/ max 20) 0d0 (* pi 2))
	    (fill-path)))))


(defparameter *alpha* 1d0)
(defparameter *beta* 1d0)

(defun hamiltonian (positions)
  (labels ((gref (arr i)
;	     (print (list 'i i))
	     (grid:gref arr i))
	   (^2 (x) (* x x))
	   (^3 (x) (* x x x))
	   (squared-distance (atom-i atom-j)
	     (+ (^2 (- (gref positions (* 2 atom-i)) (gref positions (* 2 atom-j))))
		(^2 (- (gref positions (1+ (* 2 atom-i))) (gref positions (1+ (* 2 atom-j)))))))
	   (vector-abs (atom-i)
	     (+ (^2 (gref positions (* 2 atom-i)))
		(^2 (gref positions (1+ (* 2 atom-i)))))))
    (iter (with n = (/ (first (grid:dimensions positions)) 2))
;	  (print n)
	  (with res = 0d0)
	  (for i from 0 below n)
	  (iter (for j from 0 below i)
		(incf res (/ *alpha* (^3 (squared-distance i j)))))
	  (incf res (* *beta* (vector-abs i)))
	  (finally (return res)))))

(defun brush-derivative (hair-pos output)
  (flet ((^2 (x) (* x x)))
    (iter (with n = (/ (first (grid:dimensions hair-pos)) 2))
	  (for i below n)
	  (for deriv-x = 0d0)
	  (for deriv-y = 0d0)
	  (for x-i = (grid:gref hair-pos (* 2 i)))
	  (for y-i = (grid:gref hair-pos (1+ (* 2 i))))
	  (iter (for j below n)
		(if (= i j) (next-iteration))
		(for x-j = (grid:gref hair-pos (* 2 j)))
		(for y-j = (grid:gref hair-pos (1+ (* 2 j))))
		(for dist^2 = (+ (^2 (- x-i x-j)) (^2 (- y-i y-j))))
		(for coef = (/ (* -6.0d0 *alpha* ) (^2 (^2 dist^2))))
		(incf deriv-x (* (- x-i x-j) coef))
		(incf deriv-y (* (- y-i y-j) coef)))
	  (incf deriv-x (* 2 *beta* x-i))
	  (incf deriv-y (* 2 *beta* y-i))
	  (setf (grid:gref output (* 2 i)) deriv-x
		(grid:gref output (1+ (* 2 i))) deriv-y))))
	

(defun find-minimal-configuration (nhair &optional (method +simplex-nelder-mead-on2+) (print-steps t))
  (let* ((n (* 2 nhair))
	 (step-size (grid:make-foreign-array 'double-float :dimensions n))
	 (initial (grid:make-foreign-array 'double-float :dimensions n)))
    (set-all step-size 1.0d0)
    (iter (for i from 0 below n)
	  (setf (grid:gref initial i) (random 1d0)))
    (let ((minimizer
	   (make-multi-dimensional-minimizer-f
	    method n 'hamiltonian
	    initial step-size nil)))
      (iter (with status = T)
	    (with size)
	    (for iter from 0 below (* 2000 nhair))
	    (while status)
	    (iterate minimizer)
	    (setf size (size minimizer)
		  status (not (min-test-size size 1.0d-6)))
	 (finally
	  (print 'finally)
	  (print (list 'iter iter))
	  (return
	    (list
	     (hamiltonian (solution minimizer))
	     (iter (with x = (solution minimizer))
		   (for i from 0 below (/ (grid:total-size x) 2))
		   (collecting (list (grid:gref x (* 2 i)) (grid:gref x (1+ (* 2 i)))))))))))))

(defun ham-and-derivative
    (arguments-gv-pointer value-pointer derivative-gv-pointer)
  (prog1
      (setf (grid:gref value-pointer 0)
	    (hamiltonian arguments-gv-pointer))
    (brush-derivative
     arguments-gv-pointer derivative-gv-pointer)))

(defun brush-configuration-with-derivative (nhair)
  "This is an example solving the multidimensional minimization problem
   of a paraboloid using the derivative.  The callback functions
   paraboloid-vector and paraboloid-derivative expect vectors.
   Contrast this with multimin-example-derivative-scalars, which
   expects and returns the scalar components."

  (let* ((n (* 2 nhair))
	 (step-size (grid:make-foreign-array 'double-float :dimensions n))
	 (initial (grid:make-foreign-array 'double-float :dimensions n)))
    (set-all step-size 1.0d0)
    (iter (for i from 0 below n)
	  (setf (grid:gref initial i) (random 1d0)))
  (let* ((minimizer
	  (make-multi-dimensional-minimizer-fdf
	   +conjugate-fletcher-reeves+ n
	   '(hamiltonian brush-derivative ham-and-derivative)
	   initial 0.01d0 1.0d-4 nil)))
    (iter (with status = T)
	  (for iter from 0 below 500)
	  (while status)
	  (print status)
	  (iterate minimizer)
	  (setf status
		(not (min-test-gradient
		      (mfdfminimizer-gradient minimizer)
		      1.0d-4)))
	 (finally
	  (print 'finally)
	  (print (list 'iter iter))
	  (return
	    (list (hamiltonian (solution minimizer))
		  (iter (with x = (solution minimizer))
			(for i from 0 below (/ (grid:total-size x) 2))
			(collecting (list (grid:gref x (* 2 i)) (grid:gref x (1+ (* 2 i)))))))))))))

(defun multistart (nhair)
  (cadar (sort (iter (repeat 20)
		     (collecting (brush-configuration-with-derivative nhair)))
	       #'< :key #'first)))
