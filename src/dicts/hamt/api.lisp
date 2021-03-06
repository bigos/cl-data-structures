(in-package :cl-ds.dicts.hamt)


(defclass functional-hamt-dictionary (hamt-dictionary
                                      cl-ds:functional)
  ())


(defclass mutable-hamt-dictionary (hamt-dictionary
                                   cl-ds:mutable)
  ())


(-> make-functional-hamt-dictionary ((-> (t) fixnum)
                                     (-> (t t) boolean)
                                     &key (:max-depth (integer 1 11)))
    functional-hamt-dictionary)
(defun make-functional-hamt-dictionary (hash-fn equal-fn &key (max-depth 8))
  "@b(Arguments and Values:)
   @begin(list)
   @item(hash-fn -- function that will be used to hash keys. Should return fixnum and follow all rules of hashing.)
   @item(equal-fn -- function that will be used to resolve hash conflicts.)
   @item(max-depth -- how many levels this hamt can have at most?)
   @end(list)

   @b(Description:)
   Constructs and returns new functional-hamt-dictionary object.

   @b(Notes:)
   In theory, HAMT can use infinite length of HASH, but this implementation uses 60 oldest bits at most."
  (assert (<= max-depth 10))
  (assert (> max-depth 0))
  (assure functional-hamt-dictionary (make-instance 'functional-hamt-dictionary
                                                    :hash-fn hash-fn
                                                    :root nil
                                                    :max-depth max-depth
                                                    :equal-fn equal-fn)))


(-> make-mutable-hamt-dictionary ((-> (t) fixnum)
                                  (-> (t t) boolean)
                                  &key (:max-depth (integer 1 11)))
    functional-hamt-dictionary)
(defun make-mutable-hamt-dictionary (hash-fn equal-fn &key (max-depth 8))
  "@b(Arguments and Values:)
   @begin(list)
   @item(hash-fn -- function that will be used to hash keys. Should return fixnum and follow all rules of hashing.)
   @item(equal-fn -- function that will be used to resolve hash conflicts.)
   @item(max-depth -- how many levels this hamt can have at most?)
   @end(list)

   @b(Description:)
   Constructs and returns new mutable-hamt-dictionary object.

   @b(Notes:)
   In theory, HAMT can use infinite length of HASH, but this implementation uses 60 oldest bits at most."
  (assert (<= max-depth 10))
  (assert (> max-depth 0))
  (assure mutable-hamt-dictionary (make-instance 'mutable-hamt-dictionary
                                                 :equal-fn equal-fn
                                                 :hash-fn hash-fn
                                                 :root nil
                                                 :max-depth max-depth)))


#|
(-> make-functional-key-tree-container ((-> (t) fixnum)
                                        positive-fixnum
                                        &key (:max-depth positive-fixnum) (:shallow boolean))
    functional-key-tree-container)
(let ((empty-box (make-instance 'box-node))) ;default node, avoid allocating empty nodes without reason
  (defun make-functional-key-tree-container (hash-fn &key (max-depth 8) (shallow t))
    (make-instance 'functional-key-tree-container
                   :equal-fn #'eq
                   :hash-fn hash-fn
                   :max-depth max-depth
                   :root (make-instance 'hash-node)
                   :shallow shallow
                   :remove-fn (lambda (item node equal) (declare (ignore node equal item))
                                (values empty-box t))
                   :last-node-fn (lambda (item node equal)
                                   (declare (ignore item equal))
                                   (read-content node))
                   :insert-fn (lambda (item node equal) (declare (ignore node equal))
                                (make-instance 'box-node
                                               :content (cadr item))))))
|#


(-> hamt-dictionary-at (hamt-dictionary t) (values t boolean))
(defun hamt-dictionary-at (container location)
  (declare (optimize (debug 3)))
  "Implementation of AT"
  (with-hash-tree-functions container
    (let* ((hash (hash-fn location))
           (root (access-root container)))
      (hash-do
          (node index  c
                (multiple-value-bind (r f) (try-find location
                                                     (access-conflict node)
                                                     :test (read-equal-fn container)
                                                     :key #'car)
                  (values (cdr r) f))
                (values nil nil))
          (root hash)))))


(-> functional-hamt-dictionary-erase (functional-hamt-dictionary t)
    (values functional-hamt-dictionary boolean))
(defun functional-hamt-dictionary-erase (container location)
  "Implementation of ERASE"
  (let ((old-value nil))
    (with-hash-tree-functions container
      (multiple-value-bind (new-root removed)
          (modify-copy-hamt (access-root container)
                            (hash-fn location)
                            container
                            (lambda (bottom)
                              (multiple-value-bind (list removed value)
                                  (try-remove location (and bottom (access-conflict bottom))
                                              :test (read-equal-fn container)
                                              :key #'car)
                                (setf old-value (cdr value))
                                (values (if removed
                                            (and list (make-conflict-node list))
                                            bottom)
                                        removed))))
        (values (if removed (make-instance (type-of container)
                                           :hash-fn (read-hash-fn container)
                                           :root new-root
                                           :equal-fn (read-equal-fn container)
                                           :max-depth (read-max-depth container)
                                           :size (1- (access-size container)))
                    container)
                removed
                old-value)))))


(-> functional-hamt-dictionary-insert (functional-hamt-dictionary t t)
    (values functional-hamt-dictionary boolean t))
(defun functional-hamt-dictionary-insert (container location new-value)
  "Implementation of INSERT"
  (let ((old nil)
        (rep nil))
    (with-hash-tree-functions container
      (let ((result (modify-copy-hamt (access-root container)
                                      (hash-fn location)
                                      container
                                      (lambda (bottom)
                                        (multiple-value-bind (next-list replaced old-value)
                                            (insert-or-replace (and bottom (access-conflict bottom))
                                                               (list* location new-value)
                                                               :test (read-equal-fn container)
                                                               :list-key #'car
                                                               :item-key #'car)
                                          (setf old (cdr old-value)
                                                rep replaced)
                                          (values (make-conflict-node next-list)
                                                  t))))))
        (values (make-instance (type-of container)
                               :equal-fn (read-equal-fn container)
                               :hash-fn (read-hash-fn container)
                               :root result
                               :max-depth (read-max-depth container)
                               :size (1+ (access-size container)))
                rep
                old)))))


(-> functional-hamt-dictionary-insert! (functional-hamt-dictionary t t)
    (values functional-hamt-dictionary boolean t))
(defun mutable-hamt-dictionary-insert! (container location new-value)
  "Implementation of (SETF AT)"
  (let ((replaced nil)
        (old-value nil))
    (flet ((destructive-insert (node)
             (multiple-value-bind (next-list r v)
                 (insert-or-replace (access-conflict node)
                                    (list* location new-value)
                                    :test (read-equal-fn container)
                                    :list-key #'car
                                    :item-key #'car)
               (setf (access-conflict node) next-list
                     replaced r
                     old-value (cdr v))
               node)))
      (with-hash-tree-functions container
        (let* ((prev-node nil)
               (prev-index 0)
               (hash (hash-fn location))
               (root (access-root container))
               (result (block result (with-hash-tree-functions container
                                       (hash-do (node index c) (root hash)
                                         (symbol-macrolet ((just-node (return-from result
                                                                        (progn
                                                                          (hash-node-insert! node
                                                                                             index
                                                                                             (make-conflict-node (list (list* location new-value))))
                                                                          root))))
                                           (cond+ (node prev-node (typep node 'bottom-node))
                                             ((t t t) (return-from result
                                                        (progn
                                                          (hash-node-replace! prev-node
                                                                              prev-index
                                                                              (rebuild-rehashed-node container
                                                                                                     (1+ c)
                                                                                                     (read-max-depth container)
                                                                                                     (destructive-insert node)))
                                                          root)))
                                             ((t nil t)  (return-from result
                                                           (rebuild-rehashed-node container
                                                                                  (1+ c)
                                                                                  (read-max-depth container)
                                                                                  (destructive-insert node))))
                                             ((nil nil nil) (return-from result
                                                              (make-conflict-node (list (list* location new-value)))))
                                             ((nil t nil) (return-from result
                                                            (progn (hash-node-insert! prev-node
                                                                                      prev-index
                                                                                      (rebuild-rehashed-node container
                                                                                                             (1+ c)
                                                                                                             (read-max-depth container)
                                                                                                             (make-conflict-node (list (list* location new-value)))))
                                                                   root)))
                                             ((t t nil) nil)
                                             ((t nil nil) nil)))
                                         (setf prev-node node
                                               prev-index index))))))
          (setf (access-root container) result)
          (unless replaced
            (incf (access-size container)))
          (values container
                  replaced
                  old-value))))))


(-> functional-hamt-dictionary-update (functional-hamt-dictionary t t)
    (values functional-hamt-dictionary boolean t))
(defun functional-hamt-dictionary-update (container location new-value)
  "Implementation of UPDATE"
  (let ((old nil)
        (up nil))
    (with-hash-tree-functions container
      (let ((result (modify-copy-hamt (access-root container)
                                      (hash-fn location)
                                      container
                                      (lambda (bottom)
                                        (multiple-value-bind (next-list replaced old-value)
                                            (insert-or-replace (and bottom (access-conflict bottom))
                                                               (list* location new-value)
                                                               :test (read-equal-fn container)
                                                               :list-key #'car
                                                               :item-key #'car)
                                          (setf old (cdr old-value)
                                                up replaced)
                                          (if replaced
                                              (values (make-conflict-node next-list)
                                                      t)
                                              (values bottom
                                                      nil)))))))
        (values (if up
                    (make-instance (type-of container)
                                   :equal-fn (read-equal-fn container)
                                   :hash-fn (read-hash-fn container)
                                   :root result
                                   :max-depth (read-max-depth container)
                                   :size (access-size container))
                    container)
                up
                old)))))

(-> functional-hamt-dictionary-add (functional-hamt-dictionary t t)
    (values functional-hamt-dictionary boolean t))
(defun functional-hamt-dictionary-add (container location new-value)
  "Implementation of ADD"
  (let ((add nil)
        (existing-value nil))
    (with-hash-tree-functions container
      (let ((result (modify-copy-hamt (access-root container)
                                      (hash-fn location)
                                      container
                                      (lambda (bottom)
                                        (let* ((list (and bottom (access-conflict bottom)))
                                               (item (find location list
                                                           :key #'car
                                                           :test (read-equal-fn container))))
                                          (setf add (not item)
                                                existing-value (cdr item))
                                          (if item
                                              (values bottom nil)
                                              (values (make-conflict-node (cons (list* location new-value)
                                                                                list))
                                                      t)))))))
        (values (if add
                    (make-instance (type-of container)
                                   :equal-fn (read-equal-fn container)
                                   :hash-fn (read-hash-fn container)
                                   :root result
                                   :max-depth (read-max-depth container)
                                   :size (1+ (access-size container)))
                    container)
                add
                existing-value)))))


(-> mutable-hamt-dictionary-update! (functional-hamt-dictionary t t)
    (values functional-hamt-dictionary boolean t))
(defun mutable-hamt-dictionary-update! (container location new-value)
  "Implementation of UPDATE!"
  (with-hash-tree-functions container
    (multiple-value-bind (r f)
        (let* ((hash (hash-fn location))
               (root (access-root container))
               (node (hash-do (node index) (root hash))))
          (if (typep node 'bottom-node)
              (try-find location
                        (access-conflict node)
                        :test (read-equal-fn container)
                        :key #'car)
              (values nil nil)))
      (let ((old-value (cdr r)))
        (when f
          (setf (cdr r) new-value))
        (values container f old-value)))))


(-> hamt-dictionary-size (hamt-dictionary) positive-fixnum)
(defun hamt-dictionary-size (container)
  "Implementation of SIZE"
  (access-size container))


#|

Methods. Those will just call non generic functions.

|#


(defmethod cl-ds:update! ((container mutable-hamt-dictionary) location new-value)
  (mutable-hamt-dictionary-update! container location new-value))


(defmethod cl-ds:size ((container hamt-dictionary))
  (hamt-dictionary-size container))


(defmethod cl-ds:at ((container hamt-dictionary) location)
  (hamt-dictionary-at container location))


(defmethod cl-ds:update ((container functional-hamt-dictionary) location new-value)
  (functional-hamt-dictionary-update container location new-value))


(defmethod cl-ds:add ((container functional-hamt-dictionary) location new-value)
  (functional-hamt-dictionary-add container location new-value))


(defmethod (setf cl-ds:at) (new-value (container mutable-hamt-dictionary) location)
  (mutable-hamt-dictionary-insert! container location new-value))


(defmethod cl-ds:insert ((container functional-hamt-dictionary) location new-value)
  (functional-hamt-dictionary-insert container location new-value))


(defmethod cl-ds:erase ((container functional-hamt-dictionary) location)
  (functional-hamt-dictionary-erase container location))

