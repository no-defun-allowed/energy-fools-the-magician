(in-package :jvm-class-format)

;;; See https://docs.oracle.com/javase/specs/jvms/se15/html/jvms-4.html#jvms-4.5

(define-record field ()
  ((access-flags :type access-flags)
   (name         :type jvm-constants:constant)
   (descriptor   :type jvm-constants:constant)
   (attributes   :type (pool attributes))))
