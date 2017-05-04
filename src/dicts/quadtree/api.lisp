(in-package :cl-ds.dicts.quadtree)

(defclass functional-quadtree (cl-containers:quad-tree
                               cl-ds:functional)
  ())


(defclass mutable-quadtree (cl-containers:quad-tree
                               cl-ds:mutable)
  ())

(defmethod cl-ds:at ((container quadtree) location))
(defmethod (setf cl-ds:at) (new-value (container mutable-quadtree) location))

(defmethod cl-ds:insert ((container functional-quadtree) location new-value))
(defmethod cl-ds:add ((container functional-quadtree) location new-value))
(defmethod cl-ds:update ((container functional-quadtree) location new-value))
(defmethod cl-ds:erase ((container functional-quadtree) location))
