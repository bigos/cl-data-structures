(in-package :cl-ds.dicts.quadtree)

(defclass functional-quadtree (cl-containers:quad-tree
                               cl-ds:functional)
  ())


(defclass mutable-quadtree (cl-containers:quad-tree
                               cl-ds:mutable)
  ())


;; direct methods from cl-containers
;; METABANG.CL-CONTAINERS:FIND-ITEM
;; METABANG.CL-CONTAINERS:EMPTY!
;; METABANG.CL-CONTAINERS:EMPTY-P
;; METABANG.CL-CONTAINERS:INSERT-ITEM
;; METABANG.CL-CONTAINERS:MAKE-NODE-FOR-CONTAINER
;; size
;; (setf size) ??????

;;; I have a list of operations on quadtree but I am not any wiser how I am supposed to add it to cl-data-structures


;; Some generics from src/api/generics.lisp were not implemented in
;; hamt-dictionary, I have copied the defmethods from there now I need to find
;; what is applicable for quadrees

(defmethod cl-ds:at ((container quadtree) location))
(defmethod (setf cl-ds:at) (new-value (container mutable-quadtree) location))

(defmethod cl-ds:add ((container functional-quadtree) location new-value))
;;; no add!
(defmethod cl-ds:insert ((container functional-quadtree) location new-value))
(defmethod cl-ds:erase ((container functional-quadtree) location))
;;; no erase!
(defmethod cl-ds:size ((container quadtree)))
(defmethod cl-ds:update ((container functional-quadtree) location new-value))
(defmethod cl-ds:update! ((container mutable-quadtree) location new-value))
;;; no become-functional
;;; no become-mutable
;;; no mutable-p
;;; no functional-p
;;; no empty-p
