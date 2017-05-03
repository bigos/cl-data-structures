(defpackage :cl-data-structures.dicts
  (:use :common-lisp)
  (:nicknames :cl-ds.dicts)
  (:export
   :dictionary))


(defpackage :cl-data-structures.dicts.hamt
  (:use :common-lisp :iterate :alexandria :serapeum :cl-ds.utils)
  (:nicknames :cl-ds.dicts.hamt)
  (:shadowing-import-from :iterate :collecting :summing :in)
  (:export

   :read-max-depth

   :make-functional-hamt-dictionary
   :make-mutable-hamt-dictionary

   :hamt-dictionary-at
   :hamt-dictionary-size

   :mutable-hamt-dictionary-insert!
   :mutable-hamt-dictionary-update!
   :mutable-hamt-dictionary-add!

   :functional-hamt-dictionary-insert
   :functional-hamt-dictionary-add
   :functional-hamt-dictionary-erase
   :functional-hamt-dictionary-update))


(defpackage :cl-data-structures.dicts.quadtree
  (:use :common-lisp :iterate :alexandria :serapeum :cl-ds.utils)
  (:nicknames :cl-ds.dicts.quadtree)
  (:shadowing-import-from :iterate :collecting :summing :in)
  (:export

   ;; JP, no idea what to export yet

   ;; :read-max-depth

   ;; :make-functional-quadtree-dictionary
   ;; :make-mutable-quadtree-dictionary

   ;; :quadtree-dictionary-at
   ;; :quadtree-dictionary-size

   ;; :mutable-quadtree-dictionary-insert!
   ;; :mutable-quadtree-dictionary-update!
   ;; :mutable-quadtree-dictionary-add!

   ;; :functional-quadtree-dictionary-insert
   ;; :functional-quadtree-dictionary-add
   ;; :functional-quadtree-dictionary-erase
   ;; :functional-quadtree-dictionary-update
   ))
