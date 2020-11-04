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

(defun add-jvm-instructions (basic-block &rest instructions)
  (dolist (instruction instructions)
    (add-jvm-instruction basic-block instruction)))

(defgeneric emit-instruction (instruction basic-block compiler-state))
(defmethod emit-instruction
    ((instruction cleavir-ir:top-level-enter-instruction)
     basic-block compiler-state))

(defmethod emit-instruction
    ((instruction cleavir-ir:assignment-instruction) basic-block compiler-state)
  (with-inputs/outputs ((input) (output) instruction)
    (emit-load input basic-block compiler-state)
    (emit-save output basic-block compiler-state)))

(defvar *object-class* (jvm-constants:class-named "java/lang/Object"))
(defvar *fixnum-value* (jvm-constants:method (long-class-name "Fixnum")
                                             "value" "J"))

(defgeneric emit-fixnum-value (location basic-block compiler-state)
  (:method ((constant cleavir-ir:constant-input) basic-block compiler-state)
    (add-jvm-instruction basic-block
                         (jvm-opcodes:constant-short (cleavir-ir:value constant))))
  (:method (location basic-block compiler-state)
    (emit-load location basic-block compiler-state)
    (add-jvm-instructions basic-block
                          (jvm-opcodes:invoke-virtual *fixnum-value*)
                          (jvm-opcodes:long->int))))

    
(defmethod emit-instruction
    ((instruction cleavir-ir:initialize-return-values-instruction)
     basic-block compiler-state)
  (with-inputs ((values) instruction)
    (emit-fixnum-value values basic-block compiler-state)
    (add-jvm-instructions basic-block
                          (jvm-opcodes:new-object-array *object-class*)
                          (jvm-opcodes:object-set +return-values+))))
(defmethod emit-instruction
    ((instruction cleavir-ir:set-return-value-instruction)
     basic-block compiler-state)
  (with-inputs ((index value) instruction)
    (add-jvm-instruction basic-block
                         (jvm-opcodes:object-local +return-values+)) 
    (emit-fixnum-value index basic-block compiler-state)
    (emit-load value basic-block compiler-state)
    (add-jvm-instruction basic-block
                         (jvm-opcodes:object-aset))))

(defmethod emit-instruction
    ((instruction cleavir-ir:return-instruction)
     basic-block compiler-state)
  (add-jvm-instructions basic-block
                        (jvm-opcodes:object-local +return-values+)
                        (jvm-opcodes:object-return)))
