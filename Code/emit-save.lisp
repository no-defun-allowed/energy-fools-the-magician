(in-package :hir-to-jvm)

(defgeneric emit-save (location basic-block compiler-state))

(defmethod emit-save ((location cleavir-ir:lexical-location) basic-block compiler-state)
  (add-jvm-instruction basic-block
                       (jvm-opcodes:object-set
                        (allocate-lexical location compiler-state))))
