(in-package :hir-to-jvm)

(defgeneric emit-instruction (instruction basic-block compiler-state))
