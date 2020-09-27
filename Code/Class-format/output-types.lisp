(in-package :jvm-opcodes)

(defmacro define-type (name length (value) &body body)
  `(progn
     (defmethod type-length ((#:name (eql ',name))) ,length)
     (defmethod render-value-of-type (,value (#:name (eql ',name)))
       ,@body)))

(defgeneric type-length (type-name))
(defgeneric render-value-of-type (value type-name))
