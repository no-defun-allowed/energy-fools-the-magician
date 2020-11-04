(in-package :jvm-class-format)

;;; See https://docs.oracle.com/javase/specs/jvms/se15/html/jvms-4.html#jvms-4.5

(define-record field ()
  ((access-flags :type access-flags :initarg :access-flags)
   (name         :type jvm-constants:constant :initarg :name)
   (descriptor   :type jvm-constants:constant :initarg :descriptor)
   (attributes   :type (pool attributes) :initarg :attributes)))
