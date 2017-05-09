(in-package :cl-user)

;; (push "~/Programming/Lisp/cl-data-structures/" asdf:*central-registry*)
(ql:quickload :cl-data-structures)

(setf prove:*enable-colors* nil)

(defpackage quadtree-test-suite
  (:use :cl :prove :cl-ds )
  (:shadowing-import-from :iterate :collecting :summing :in)
  (:export
           :run-suite))

(in-package :quadtree-test-suite)

(plan 3)

(ok (not (find 4 '(1 2 3))))
(is 4 4)
(isnt 1 #\1)

(finalize)
