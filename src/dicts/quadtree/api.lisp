(in-package :cl-ds.dicts.quadtree)

(defclass functional-quadtree (cl-containers:quad-tree
                               cl-ds:functional)
  ())


(defclass mutable-quadtree (cl-containers:quad-tree
                               cl-ds:mutable)
  ())

(defun unimplemented () (error "not implemented"))

(defun make-functional-quad-tree ()
  (unimplemented))

(defun make-mutable-quad-tree ()
  (unimplemented))

;; at -> return item at coordinates (coordinates should be t, for tests array of size = 2 will be ok)
;; insert -> insert item at coordinates, replace if exists
;; add -> insert item at coordinates if place is not occupied
;; update -> replace item at coordinates, do nothing otherwise
;; erase -> you know the drill


;; | cl-containers | cl-data-structures |
;; | find-item     | at                 |
;; | insert-item   | insert             |
;; |               | add                |
;; |               | update             |
;; | empty!        | erase              |

(defmethod cl-ds:at ((container functional-quadtree) location)
  (unimplemented))

(defmethod (setf cl-ds:at) (new-value (container mutable-quadtree) location)
  (unimplemented))

(defmethod cl-ds:insert ((container functional-quadtree) location new-value)
  (unimplemented))

(defmethod cl-ds:add ((container functional-quadtree) location new-value)
  (unimplemented))

(defmethod cl-ds:update ((container functional-quadtree) location new-value)
  (unimplemented))

(defmethod cl-ds:erase ((container functional-quadtree) location)
  (unimplemented))
