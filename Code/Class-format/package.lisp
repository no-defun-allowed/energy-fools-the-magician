(defpackage :jvm-opcodes
  (:use :cl :jvm-binary-output)
  (:shadow #:method)
  (:export #:instruction
           #:instruction-length
           #:render-instruction))

(defpackage :jvm-constants
  (:use :cl :jvm-opcodes)
  (:export #:constant-pool #:*constant-pool*
           #:make-constant-pool
           #:class-named #:class-file))
