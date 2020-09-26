(in-package :hir-to-jvm)

(defclass compiler-state ()
  ((lexicals :initform 0 :accessor lexical-count)
   (basic-blocks :initform (make-hash-table)
                 :reader basic-blocks)
   (basic-block-positions :initform (make-hash-table)
                          :reader basic-block-positions)
   (basic-block-ordering :initform (make-array 0
                                               :adjustable t
                                               :fill-pointer 0)
                         :reader basic-block-ordering)))

(defun assemble-hir (initial-instruction compiler-state)
  (let ((work-list (list (make-instance 'basic-block
                          :initial-instruction initial-instruction))))
    (loop until (null work-list)
          do (let ((basic-block (pop work-list)))
               (vector-push-extend basic-block
                                   (basic-block-ordering compiler-state))
               (setf work-list
                     (append (assemble-basic-block basic-block compiler-state)
                             work-list))))
    compiler-state))

(defvar *compiler*)
(defvar *this-position*)
