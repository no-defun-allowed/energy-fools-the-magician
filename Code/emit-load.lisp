(in-package :hir-to-jvm)

(defgeneric emit-load (location basic-block compiler-state))

(defmethod emit-load ((location cleavir-ir:constant-input) basic-block compiler-state)
  (add-jvm-instructions basic-block
                        (jvm-opcodes:object-local +self+)
                        (jvm-opcodes:static-ref (allocate-constant (cleavir-ir:value location)
                                                                   compiler-state))))
(defmethod emit-load ((location cleavir-ir:lexical-location) basic-block compiler-state)
  (add-jvm-instruction basic-block
                       (jvm-opcodes:object-local
                        (allocate-lexical location compiler-state))))
