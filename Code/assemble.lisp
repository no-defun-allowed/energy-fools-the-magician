(in-package :hir-to-jvm)

(defclass compiler-state ()
  ((lexicals :initform (make-array 0
                                   :adjustable t
                                   :fill-pointer 0)
             :accessor lexicals)
   (basic-blocks :initform (make-hash-table)
                 :reader basic-blocks)
   (basic-block-positions :initform (make-hash-table)
                          :reader basic-block-positions)
   (basic-block-ordering :initform (make-array 0
                                               :adjustable t
                                               :fill-pointer 0)
                         :reader basic-block-ordering)))

(defun lexical-count (compiler-state)
  (length (lexicals compiler-state)))

(defun %assemble-hir (initial-instruction compiler-state)
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

(defun assign-basic-block-positions (compiler-state)
  (loop with positions = (basic-block-positions compiler-state)
        for basic-block across (basic-block-ordering compiler-state)
        for position = 0 then (+ position (basic-block-length basic-block))
        do (setf (gethash basic-block positions)
                 position)))

(defun assemble-hir (initial-instruction)
  (let ((compiler-state (make-instance 'compiler-state)))
    (%assemble-hir initial-instruction compiler-state)
    (assign-basic-block-positions compiler-state)
    compiler-state))

(defvar *compiler*)
(defvar *this-position*)
