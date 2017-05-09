(declaim (optmise (debug 3)))

(in-package :cl-ds.trees.quadtree)

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




;; * Classes
;; ** quad-tree
;; [ ]  CLASSIFIER = CONS-CLASSIFIER
;; [ ]  KEY        = IDENTITY
;; [ ]  ROOT       = #<QUAD-TREE-NODE (0 . 0)>
;; [ ]  SIZE       = 11
;; [ ]  TEST       = EQUAL

;; ** quad-tree-node
;; [ ]  BOTTOM-LEFT-CHILD  = #<QUAD-TREE-NODE (-1 . -1)>
;; [ ]  BOTTOM-RIGHT-CHILD = #<QUAD-TREE-NODE (1 . -1)>
;; [ ]  ELEMENT            = (0 . 0)
;; [ ]  PARENT             = NIL
;; [ ]  TOP-LEFT-CHILD     = #<QUAD-TREE-NODE (-1 . 1)>
;; [ ]  TOP-RIGHT-CHILD    = #<QUAD-TREE-NODE (3.1 . 3.1)>
;; [ ]  TREE               = #<QUAD-TREE {1002E10523}>




;;; finish the classes
(defclass quadtree ()
  ((size 0)))

(defclass quad-tree-node ()
  ((tree :initform nil
         :initarg :tree
         :accessor tree))
  (parent))

(defun cons-classifier (x y)
  (cond ((and (< (car x) (car y))
              (>= (cdr x) (cdr y)))
         :TOP-LEFT)
        ((and (>= (car x) (car y))
              (>= (cdr x) (cdr y)))
         :TOP-RIGHT)
        ((and (>= (car x) (car y))
              (< (cdr x) (cdr y)))
         :BOTTOM-RIGHT)
        ((and (< (car x) (car y))
              (< (cdr x) (cdr y)))
         :BOTTOM-LEFT)
        (T (error "ran out of options"))))

(defun make-quadtree ()
  (make-instance 'quadtree
                 :classifier 'cons-classifier))
