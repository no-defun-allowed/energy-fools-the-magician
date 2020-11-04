(defpackage :jvm-class-format
  (:use :cl :jvm-binary-output)
  (:shadow #:method)
  (:export #:class-file #:method #:field
           #:attribute #:code-attribute))

(defpackage :jvm-constants
  (:use :cl :jvm-binary-output)
  (:shadow #:method)
  (:export #:constant #:constant-pool #:*constant-pool*
           #:make-constant-pool
           #:with-constant-pool
           #:class-named #:constant-utf8 #:field #:method #:constant-long))

(defpackage :jvm-opcodes
  (:use :cl :jvm-binary-output)
  (:shadow #:method)
  (:import-from :jvm-constants #:constant)
  (:export #:instruction #:instruction-length #:render-instruction
           #:address))
