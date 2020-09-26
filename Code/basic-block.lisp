(in-package :hir-to-jvm)

(defclass basic-block ()
  ((initial-instruction :initarg :initial-instruction
                        :reader initial-instruction) 
   (hir-instructions :initform (make-array 1 :adjustable t :fill-pointer 0)
                     :reader hir-instructions)
   (jvm-instructions :initform (make-array 1 :adjustable t :fill-pointer 0)
                     :reader jvm-instructions)))
(defmethod initialize-instance :after ((basic-block basic-block) &key)
  (vector-push-extend (initial-instruction basic-block)
                      (hir-instructions basic-block)))

(defgeneric ends-basic-block-p (instruction)
  (:method ((instruction cleavir-ir:one-successor-mixin))
    (values nil '()))
  (:method ((instruction cleavir-ir:no-successors-mixin))
    (values t '()))
  (:method ((instruction cleavir-ir:multiple-successors-mixin))
    (values t
            (loop for successor in (cleavir-ir:successors instruction)
                  collect (list (make-instance 'basic-block
                                 :initial-instruction successor))))))

(defun assemble-basic-block (basic-block compiler-state)
  (let ((instruction (initial-instruction basic-block)))
    (setf (gethash instruction (basic-blocks compiler-state))
          basic-block)
    (loop
      (print instruction)
      (emit-instruction instruction basic-block compiler-state)
      (multiple-value-bind (ends? successors)
          (ends-basic-block-p instruction)
        (when ends?
          (return-from assemble-basic-block successors))
        (setf instruction (first (cleavir-ir:successors instruction)))
        (vector-push-extend instruction (hir-instructions basic-block))))))

(defgeneric basic-block-length (basic-block)
  (:method ((basic-block basic-block))
    (reduce #'+ (jvm-instructions basic-block)
            :key #'jvm-opcodes:instruction-length)))
