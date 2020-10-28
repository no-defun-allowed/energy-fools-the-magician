(defpackage :jvm-opcodes
  (:use :cl :jvm-binary-output)
  (:shadow #:method)
  (:export #:class-file
           #:instruction
           #:instruction-length
           #:render-instruction))

(defpackage :jvm-constants
  (:use :cl :jvm-opcodes :jvm-binary-output)
  (:export #:constant #:constant-pool #:*constant-pool*
           #:make-constant-pool #:class-named))
