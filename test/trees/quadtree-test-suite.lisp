(declaim (optimize (debug 3)))

(in-package :cl-user)

(setf prove:*enable-colors* nil)

(defpackage quadtree-test-suite
  (:use :cl :prove :cl-ds.trees.quadtree)
  (:shadowing-import-from :iterate :collecting :summing :in))

(in-package :quadtree-test-suite)

(plan 7)

(is (+ 2 2) 4)
(isnt 1 #\1)

(defparameter qt (make-quadtree))
(is (type-of qt) 'quadtree)
(is (size qt) 0)

(defparameter n1 (make-node-for-container qt (cons 0 0)))
(isnt n1 nil)
(is (type-of n1) 'quadtree-node)
(is (element n1) (cons 0 0))

(insert-item qt n1)

(finalize)
