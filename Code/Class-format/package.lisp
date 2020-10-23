(defpackage :jvm-opcodes
  (:use :cl :jvm-binary-output)
  (:shadow #:method)
  (:export #:instruction
           #:instruction-length
           #:render-instruction))
