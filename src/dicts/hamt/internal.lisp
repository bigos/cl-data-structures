(in-package :cl-ds.dicts.hamt)

#|

Basic types

|#

(deftype maybe-node ()
  `(or null hash-node bottom-node))


(deftype node-position ()
  `(values maybe-node fixnum))


(deftype hash-node-index ()
  `(integer 0 63))


(deftype just-node ()
  `(or hash-node bottom-node))

#|

Macros

|#

(defmacro hash-do ((node index &optional (count (gensym)) on-bottom-leaf on-bottom-nil)
                   (root hash &optional (max-depth 10))
                   &body body)
  "Macro used for writing code going down into hash tree."
  (with-gensyms (!pos !block !leaf)
    (once-only (hash root max-depth)
      `(block ,!block
         (assert (<= ,max-depth 10))
         (do ((,!pos 6 (+ ,!pos 6))
              (,index (ldb (byte 6 0) ,hash)
                      (ldb (byte 6 ,!pos) ,hash))
              (,count 0 (1+ ,count))
              (,!leaf (and ,root (not (hash-node-p ,root)))
                      (hash-node-contains-leaf ,node ,index))
              (,node ,root (and (hash-node-p ,node)
                                (hash-node-contains ,node ,index)
                                (hash-node-access ,node ,index))))
             ((= ,count ,max-depth)
              (values ,node
                      ,count))
           (declare (type fixnum ,hash ,!pos ,index ,count))
           (progn
             ,(when on-bottom-nil
                `(unless ,node
                   (return-from ,!block
                     ,on-bottom-nil)))
             ,(when on-bottom-leaf
                `(when ,!leaf
                   (return-from ,!block
                     ,on-bottom-leaf)))
             ,@body
             (when (or ,!leaf (null ,node))
               (return-from ,!block
                 (values ,node
                         ,count)))))))))


(defmacro with-hash-tree-functions (container &body body)
  "Simple macro adding local functions (all forwards to the container closures)."
  `(fbind ((equal-fn (read-equal-fn ,container))
           (hash-fn (read-hash-fn ,container)))
     (declare (ignorable (function hash-fn)
                         (function equal-fn)))
     ,@body))


(defmacro descend-into-hash (root max-depth hash final-fn)
  "Go into hamt, storing nodes into two stack allocated arrays. Pass those into supplied function. Macro so it is always inlined.
   @b(Arguments and values)
   @begin(list)
     @item(root -- node where we are starting descending)
     @item(max-depth -- how many nodes we are going to scan at most? @b(Must) be lower than 11).
     @item(hash -- fixnum).
     @item(final-fn -- symbol bound to function. Will ).
   @end(list)"
  (once-only (max-depth)
    `(let ((path (make-array 11))
           (indexes (make-array ,max-depth :element-type 'fixnum))
           (depth 0))
       (declare (type fixnum depth)
                (dynamic-extent path indexes depth))
       (hash-do
           (node index i)
           (,root ,hash ,max-depth)
         (setf (aref path i) node
               (aref indexes i) index)
         (incf depth))
       (,final-fn path indexes depth)))) ;finally, pass data into function. Depth informs user about how many nodes was really scanned.


(defmacro set-in-leaf-mask (node position bit)
  `(setf (ldb (byte 1 ,position) (hash-node-leaf-mask ,node)) ,bit))


(defmacro set-in-node-mask (node position bit)
  `(setf (ldb (byte 1 ,position) (hash-node-node-mask ,node)) ,bit))


#|

Tree structure of HAMT

|#


(defstruct hash-node
  (leaf-mask 0 :type (unsigned-byte 64))
  (node-mask 0 :type (unsigned-byte 64))
  (content #() :type simple-array))


(declaim (inline make-hash-node))


(defclass bottom-node () ()
  (:documentation "Base class of the last (conflict) node. Subclasses present to dispatch relevant logic."))


(defclass conflict-node (bottom-node)
  ((%conflict :initarg :conflict
              :accessor access-conflict
              :initform (list)
              :type list
              :documentation "List of elements with conflicting hash."))
  (:documentation "Conflict node simply holds list of elements that are conflicting."))


(-> make-conflict-node (list) conflict-node)
(defun make-conflict-node (content)
  (assure conflict-node (make-instance 'conflict-node :conflict content)))


(defclass box-node (bottom-node)
  ((%content :initarg :content
             :reader read-content
             :documentation "Internal value of box"))
  (:documentation "Box node holds only one element inside."))


(defgeneric empty-node-p (bottom-node))


(defgeneric contains-p (bottom-node item fn))


(defmethod contains-p ((node conflict-node) item fn)
  (find item (access-conflict node) :test fn))


(defmethod empty-node-p ((node box-node))
  (slot-boundp node '%content))


(defmethod empty-node-p ((node conflict-node))
  (endp (access-conflict node)))


(define-constant +hash-level+ 6)

#|

Interface class.

|#

(defclass fundamental-hamt-container (cl-ds:fundamental-container)
  ((%root :type (or hash-node bottom-node null)
          :accessor access-root
          :initarg :root
          :documentation "Hash node pointing to root of the whole hash tree.")
   (%hash-fn :type (-> (x) fixnum)
             :reader read-hash-fn
             :initarg :hash-fn
             :documentation "Closure used for key hashing. Set by the user.")
   (%equal-fn :type (-> (t t) boolean)
              :reader read-equal-fn
              :initarg :equal-fn
              :documentation "Closure used for comparing items at the bottom level lists.")
   (%max-depth :initarg :max-depth
               :type (integer 0 10)
               :reader read-max-depth
               :documentation "Maximal depth of tree.")
   (%size :initarg :size
          :initform 0
          :type positive-integer
          :accessor access-size
          :documentation "How many elements are in there?"))
  (:documentation "Base class of other containers. Acts as any container for bunch of closures (those vary depending on the concrete container) and root of the tree."))


(defclass hamt-dictionary (fundamental-hamt-container
                           cl-ds.dicts:dictionary)
  ())


#|

Functions with basic bit logic.

|#

(-> hash-node-whole-mask (hash-node) (unsigned-byte 64))
(defun hash-node-whole-mask (node)
  (logior (hash-node-node-mask node) (hash-node-leaf-mask node)))


(declaim (inline hash-node-whole-mask))


(-> hash-node-to-masked-index (hash-node (hash-node-index)) hash-node-index)
(defun hash-node-to-masked-index (hash-node index)
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0) (space 0)))
  (~>> hash-node
       hash-node-whole-mask
       (ldb (byte index 0))
       logcount))


(declaim (inline hash-node-to-masked-index))


(-> hash-node-contains (hash-node hash-node-index) boolean)
(defun hash-node-contains (hash-node index)
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0) (space 0)))
  (~>> (hash-node-whole-mask hash-node)
       (ldb (byte 1 index))
       zerop
       not))


(-> hash-node-contains-leaf (hash-node hash-node-index) boolean)
(defun hash-node-contains-leaf (hash-node index)
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0) (space 0)))
  (~>> (hash-node-leaf-mask hash-node)
       (ldb (byte 1 index))
       zerop
       not))


(-> hash-node-contains-node (hash-node hash-node-index) boolean)
(defun hash-node-contains-node (hash-node index)
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0) (space 0)))
  (~>> (hash-node-node-mask hash-node)
       (ldb (byte 1 index))
       zerop
       not))


(declaim (inline hash-node-contains))


(-> hash-node-access (hash-node hash-node-index) t)
(defun hash-node-access (hash-node index)
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0) (space 0)))
  (handler-case
      (~>> (hash-node-to-masked-index hash-node index)
           (aref (hash-node-content hash-node)))))


(declaim (inline hash-node-access))


(-> hash-node-size (hash-node) (integer 0 64))
(defun hash-node-size (node)
  (logcount (hash-node-whole-mask node)))

#|

Copy nodes and stuff.

|#

(-> hash-node-replace-in-the-copy (hash-node t hash-node-index) hash-node)
(defun hash-node-replace-in-the-copy (hash-node item index)
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0) (space 0)))
  (let* ((content (copy-array (hash-node-content hash-node)))
         (leaf-mask (hash-node-leaf-mask hash-node))
         (node-mask (hash-node-node-mask hash-node)))
    (declare (type (unsigned-byte 64) leaf-mask node-mask))
    (if (hash-node-p item)
        (setf (ldb (byte 1 index) node-mask) 1
              (ldb (byte 1 index) leaf-mask) 0)
        (setf (ldb (byte 1 index) node-mask) 0
              (ldb (byte 1 index) leaf-mask) 1))
    (setf (aref content (logcount (ldb (byte index 0) (logior leaf-mask node-mask))))
          item)
    (make-hash-node :leaf-mask leaf-mask
                    :node-mask node-mask
                    :content content)))


(declaim (inline hash-node-replace-in-the-copy))


(-> hash-node-insert-into-copy (hash-node t hash-node-index) hash-node)
(defun hash-node-insert-into-copy (hash-node item index)
  (let ((position (hash-node-to-masked-index hash-node index)))
    (with-vectors ((current-array (hash-node-content hash-node))
                   (new-array (make-array (1+ (array-dimension current-array 0)))))
      (assert (~> (array-dimension new-array 0)
                  (<= 64)))
      ;;before new element
      (iterate

        (for i from 0 below position)
        (setf (new-array i)
              (current-array i)))

      ;;new element
      (setf (new-array position)
            item)

      ;;after new element
      (iterate
        (for i from position below (array-dimension current-array 0))
        (setf (new-array (1+ i))
              (current-array i)))

      ;;just make new hash-node
      (let ((node-mask (hash-node-node-mask hash-node))
            (leaf-mask (hash-node-leaf-mask hash-node)))
        (if (hash-node-p item)
            (setf (ldb (byte 1 index) node-mask) 1)
            (setf (ldb (byte 1 index) leaf-mask) 1))
        (make-hash-node :node-mask node-mask
                        :leaf-mask leaf-mask
                        :content new-array)))))


(defun non-empty-hash-table-p (table)
  (and (typep table 'hash-table)
       (not (zerop (hash-table-count table)))))


(deftype non-empty-hash-table ()
  `(satisfies non-empty-hash-table-p))


(defgeneric rehash (container conflict level)
  (:documentation "Attempts to divide conflct into smaller ones. Retudnerd hash table maps position of conflict to conflict itself and should contain at least one element"))


(defgeneric single-elementp (conflict)
  (:documentation "Checks if conflict node holds just a single element. Returns t if it does, returns nil if it does not."))


(-> rebuild-rehashed-node (fundamental-hamt-container fixnum fixnum bottom-node) just-node)
(-> build-rehashed-node (fundamental-hamt-container fixnum fixnum hash-table) just-node)
(defun build-rehashed-node (container depth max-depth content)
  (let ((array (make-array (hash-table-count content)))
        (leaf-mask 0))
    (iterate
     (for (index conflict) in-hashtable content)
     (setf (ldb (byte 1 index) leaf-mask) 1))
    (iterate
     (for (index conflict) in-hashtable content)
     (setf (aref array (logcount (ldb (byte index 0) leaf-mask)))
           (if (or (<= depth max-depth) (single-elementp conflict))
               conflict
             (rebuild-rehashed-node container
                                    depth
                                    max-depth
                                    conflict))))
    (make-hash-node :leaf-mask leaf-mask
                    :node-mask 0
                    :content array)))


(defun rebuild-rehashed-node (container depth max-depth conflict)
  (if (or (>= depth max-depth) (single-elementp conflict))
      conflict
      (let ((table (rehash container conflict (1- depth))))
        (build-rehashed-node container (1+ depth) max-depth table))))


(-> build-node (hash-node-index just-node) hash-node)
(defun build-node (index content)
  (if (hash-node-p content)
      (make-hash-node :node-mask (ash 1 index)
                      :content (make-array 1 :initial-element content))
      (make-hash-node :leaf-mask (ash 1 index)
                      :content (make-array 1 :initial-element content))))


(-> modify-copy-hamt (maybe-node fixnum fundamental-hamt-container (-> (bottom-node) (values maybe-node boolean)))
    (values hash-node boolean))
(defun modify-copy-hamt (root hash container bottom-fn)
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0) (space 0)))
  ;; (declare (optimize debug))
  (fbind (bottom-fn)
    (flet ((cont (path indexes length) ;path and indexes have constant size BUT only part of it is used, that's why length is passed here
             (declare (type simple-array path indexes)
                      (type fixnum length))
             (with-vectors (path indexes)
               (iterate
                 (for i from (- length 1) downto 0) ;reverse order (starting from deepest node)
                 (for node = (path i))
                 (for index = (indexes i))
                 (for ac initially (let* ((last (path (1- length)))
                                          (bottom-node-or-nil (and (typep last 'bottom-node) last)))
                                     (multiple-value-bind (conflict changed) (bottom-fn bottom-node-or-nil)
                                       (unless changed
                                         (return-from modify-copy-hamt (values root nil)))
                                       (if (or (null bottom-node-or-nil)
                                               (null conflict))
                                           ;;if we didn't find element or element was found but depth was already maximal,
                                           ;;we will just return element, otherwise attempt to divide (rehash) conflicting node into few more
                                           conflict
                                           ;;rehash actually returns cl:hash-table, build-rehashed-node transforms it into another hash-node, depth is increased by 1 this way
                                           (rebuild-rehashed-node container
                                                                  length
                                                                  (read-max-depth container)
                                                                  conflict))))
                      then (cond ((null node) ac) ;no node on path, just use our conflict node
                                 ((typep node 'bottom-node) ac) ;corner case, added conflict or resolved conflict
                                 (ac (if (hash-node-contains node index)
                                         (hash-node-replace-in-the-copy node ac index)
                                         (hash-node-insert-into-copy node ac index)))
                                 (t (if (eql 1 (hash-node-size node))
                                        ac
                                        (hash-node-remove-from-the-copy node index)))))
                 (finally (return (values ac t)))))))
      (declare (dynamic-extent (function cont))
               (inline cont))
      (descend-into-hash root
                         (read-max-depth container)
                         hash
                         cont))))


(defun hash-node-insert! (node index content)
  (assert (zerop (ldb (byte 1 index) (hash-node-whole-mask node))))
  (let* ((next-size (~> node
                        hash-node-content
                        (array-dimension 0)
                        1+))
        (next-mask (~>> node
                        hash-node-whole-mask
                        (dpb 1 (byte 1 index))))
        (index (~>> next-mask
                    (ldb (byte index 0))
                    logcount)))
    (with-vectors ((n (make-array next-size)) (s (hash-node-content node)))
      (iterate
        (for i from 0 below next-size)
        (cond-compare (i index)
                      (setf (n i) (s i))
                      (setf (n i) content)
                      (setf (n i) (s (1- i)))))
      (setf (hash-node-content node) n)
      (if (hash-node-p node)
          (set-in-node-mask node index 1)
          (set-in-leaf-mask node index 1))
      node)))


(defun hash-node-replace! (node index content)
  (assert (not (zerop (ldb (byte 1 index) (hash-node-whole-mask node)))))
  (with-vectors ((a (hash-node-content node)))
    (setf (a (~>> node
                  hash-node-whole-mask
                  (ldb (byte index 0))
                  logcount))
          content)
    (if (hash-node-p content)
        (set-in-node-mask node index 1)
        (set-in-leaf-mask node index 1)))
  node)


(-> hash-node-remove-from-the-copy (hash-node fixnum) maybe-node)
(defun hash-node-remove-from-the-copy (node index)
  "Returns copy of node, but without element under index. Not safe, does not check if element is actually present."
  (and (hash-node-contains node index)
       (let ((new-array (let ((size (hash-node-size node)))
                          (unless (eql 1 size)
                            (let ((result (make-array (1- size)))
                                  (position (1- (logcount (ldb (byte (1+ index) 0)
                                                               (hash-node-whole-mask node)))))
                                  (input (hash-node-content node)))
                              (iterate
                                (for i from 0 below position)
                                (setf (aref result i) (aref input i)))
                              (iterate
                                (for i from position)
                                (for j from (1+ position) below size)
                                (setf (aref result i) (aref input j)))
                              result)))))
         (when new-array
           (make-hash-node :leaf-mask (dpb 0 (byte 1 index) (hash-node-leaf-mask node))
                           :node-mask (dpb 0 (byte 1 index) (hash-node-node-mask node))
                           :content new-array)))))


(-> map-hash-tree ((-> (bottom-node) t) hash-node) hash-node)
(defun map-hash-tree (fn root)
  (iterate
    (with stack = (make-array 32
                              :element-type 'maybe-node
                              :adjustable t
                              :fill-pointer 1
                              :initial-element root))
    (for current = (pop-last stack))
    (while current)
    (for (node . hash-path) = current)
    (etypecase node
      (bottom-node (funcall fn node))
      (hash-node (with-accessors ((mask hash-node-whole-mask)
                                  (content hash-node-content)) node
                   (iterate
                     (for i from 0 below 64)
                     (with index = 0)
                     (unless (~> (ldb (byte 1 i) mask)
                                 zerop)
                       (vector-push-extend (aref content index)
                                           stack)
                       (incf index)))))
      (t (assert (null node)))))
  root)


(-> contains-part-of-hash (fixnum fixnum (integer 0 64)) boolean)
(defun contains-part-of-hash (hash partial-hash depth)
  (~>> hash
       (logxor partial-hash)
       (ldb (byte depth 0))
       zerop))

(defmethod hash-of-bottom-node ((node conflict-node) container)
  (declare (type fundamental-hamt-container container))
  (with-hash-tree-functions container
    (~> node
        access-conflict
        caar
        hash-fn)))


(defmethod rehash ((container hamt-dictionary) conflict level)
  (declare (type conflict-node conflict))
  (let ((result (make-hash-table))
        (byte (byte +hash-level+ (* +hash-level+ level))))
    (declare (dynamic-extent byte))
    (with-hash-tree-functions container
      (iterate
       (for key.value in (access-conflict conflict))
       (for (key . value) = key.value)
       (for hash = (hash-fn key))
       (for index = (ldb byte hash))
       (push key.value (access-conflict (ensure (gethash index result)
                                                (make-instance 'conflict-node))))))
    result))


(defmethod single-elementp ((conflict conflict-node))
  (endp (cdr (access-conflict conflict))))
