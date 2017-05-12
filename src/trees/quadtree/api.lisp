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
   (key :initform #'identity :reader key)
   (root :initform nil :accessor root)
   (size :initform 0 :accessor size)
   (test :initarg :test)))

(defclass quadtree-node ()
  ((parent :initform nil :accessor parent)
   (element :initform nil
            :initarg :element
            :accessor element)
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

(defgeneric make-node-for-container (container item &key)
  (:documentation ""))

(defmethod make-node-for-container ((tree quadtree) (item t) &key)
  (if item
      (make-instance 'quadtree-node
                     :element item
                     :tree tree)
      nil))

(defgeneric node-empty-p (node))

(defmethod node-empty-p ((node quadtree-node))
  (null (element node)))

(defgeneric notify-element-of-child-status (element status)
  (:documentation "This is called to allow the element to know its
status as a child. Useful for quad tree elements, where an element's position
relative to its parent could be relevant to the element. Status is one of:
:TOP-LEFT, :TOP-RIGHT, :BOTTOM-LEFT, :BOTTOM-RIGHT or :ROOT")
  (:method ((element t) (status t))
    (values nil)))

(defgeneric insert-item (container item))
(defmethod insert-item ((tree quadtree) (item quadtree-node))
  (loop with key = (key tree)
     with y = (make-node-for-container tree nil)
     with classifier = (classifier tree)
     and x = (root tree)
     and key-item = (funcall key (element item))
     while (not (node-empty-p x))
     do
       (progn (setf x y)
              (case (funcall classifier key-item (funcall key (element x)))
                (:TOP-LEFT (setf x (top-left-child x)))
                (:TOP-RIGHT (setf x (top-right-child x)))
                (:BOTTOM-LEFT (setf x (bottom-left-child x)))
                (:BOTTOM-RIGHT (setf x (bottom-right-child x)))))
     finally
       (progn
         (setf (parent item) y
               (tree item) tree)
         (if (node-empty-p y)
             (progn
               (cerror "debug" "debug")
               (notify-element-of-child-status (element item) :ROOT)
               (setf (root tree) item))
             (case (funcall classifier key-item (funcall key (element y)))
               (:TOP-LEFT
                (notify-element-of-child-status (element item) :TOP-LEFT)
                (setf (top-left-child y) item))
               (:TOP-RIGHT
                (notify-element-of-child-status (element item) :TOP-RIGHT)
                (setf (top-right-child y) item))
               (:BOTTOM-LEFT
                (notify-element-of-child-status (element item) :BOTTOM-LEFT)
                (setf (bottom-left-child y) item))
               (:BOTTOM-RIGHT
                (notify-element-of-child-status
                 (element item) :BOTTOM-RIGHT)
                (setf (bottom-right-child y) item))))))
  (incf (size tree)))
