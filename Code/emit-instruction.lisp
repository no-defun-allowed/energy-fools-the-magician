(in-package :hir-to-jvm)

(defgeneric emit-instruction (instruction basic-block compiler-state)
  (:method ((instruction cleavir-ir:instruction) basic-block compiler-state)
    ;; do nothing for now
    ))
