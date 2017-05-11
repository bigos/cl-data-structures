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

;;; finish the slots
(defclass quadtree ()
  ((classifier :initform #'<
               :initarg :classifier
               :accessor classifier)
   (key :initform nil)
   (root :initform nil)
   (size :initform 0 :reader size)
   (test :initarg :test)))

(defclass quad-tree-node ()
  ((parent :initform nil)
   (element :initform nil)
   (tree :initform nil
         :initarg :tree
         :accessor tree)
   (top-left-child :initform nil
                   :accessor top-left-child)
   (top-right-child :initform nil
                    :accessor top-right-child)
   (bottom-left-child :initform nil
                      :accessor bottom-left-child)
   (bottom-right-child :initform nil
                       :accessor bottom-right-child)))

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
