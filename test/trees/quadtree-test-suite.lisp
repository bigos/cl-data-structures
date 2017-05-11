(in-package :cl-user)

(setf prove:*enable-colors* nil)

(defpackage quadtree-test-suite
  (:use :cl :prove :cl-ds.trees.quadtree)
  (:shadowing-import-from :iterate :collecting :summing :in)
  (:export
           :run-suite))

(in-package :quadtree-test-suite)

(plan 4)

(is (+ 2 2) 4)
(isnt 1 #\1)

(defparameter qt (make-instance 'quadtree))
(is (type-of qt) 'quadtree)
(is (size qt) 0)

(finalize)
