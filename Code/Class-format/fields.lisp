(in-package :jvm-opcodes)

;;; See https://docs.oracle.com/javase/specs/jvms/se15/html/jvms-4.html#jvms-4.5

(define-record field
  ((access-flags :type access-flags)
   (name         :type constant)
   (descriptor   :type constant)
   (attributes   :type (pool field-attributes))))
