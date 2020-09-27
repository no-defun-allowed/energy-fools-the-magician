(defpackage :jvm-opcodes
  (:use :cl)
  (:export #:instruction
           #:instruction-length
           #:render-instruction
           #:type-length
           #:render-value-of-type))
