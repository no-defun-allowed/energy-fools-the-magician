(in-package :hir-to-jvm)

(defmacro with-inputs (((&rest inputs) instruction) &body body)
  `(destructuring-bind ,inputs (cleavir-ir:inputs ,instruction)
     ,@body))
(defmacro with-outputs (((&rest outputs) instruction) &body body)
  `(destructuring-bind ,outputs (cleavir-ir:outputs ,instruction)
     ,@body))
(defmacro with-inputs/outputs (((&rest inputs) (&rest outputs) instruction)
                               &body body)
  (alexandria:once-only (instruction)
    `(with-inputs (,inputs ,instruction)
       (with-outputs (,outputs ,instruction)
         ,@body))))

(defun emit-jvm-instructions (basic-block &rest instructions)
  (dolist (instruction instructions)
    (add-jvm-instruction basic-block instruction)))

(defgeneric emit-instruction (instruction basic-block compiler-state)
  (:method ((instruction cleavir-ir:instruction) basic-block compiler-state)
    #| do nothing for now |#)
  (:method ((instruction cleavir-ir:enter-instruction) basic-block compiler-state)
    (with-outputs ((static-environment dynamic-environment) instruction)
      (emit-jvm-instructions basic-block
       (jvm-opcodes:object-local/2)     ; argument #2
       (jvm-opcodes:object-set (allocate-lexical dynamic-environment compiler-state))
       (jvm-opcodes:object-local/0)     ; this function
       (jvm-opcodes:field-ref 0)        ; static environment
       (jvm-opcodes:object-set (allocate-lexical static-environment compiler-state))))))
