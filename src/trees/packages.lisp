(defpackage :cl-data-structures.trees
  (:use :common-lisp)
  (:nicknames :cl-ds.trees)
  (:export

   ))

(defpackage :cl-data-structures.trees.quadtree
  (:use :common-lisp :iterate :alexandria :serapeum :cl-ds.utils)
  (:nicknames :cl-ds.trees.quadtree)
  (:shadowing-import-from :iterate :collecting :summing :in)
  (:export

   :quadtree


   ))
