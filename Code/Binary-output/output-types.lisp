(in-package :jvm-binary-output)

(defmacro define-type (name length (value) &body body)
  (destructuring-bind (name &rest arguments)
      (alexandria:ensure-list name)
    `(progn
       (defmethod type-length ((#:name (eql ',name)) arguments)
         (destructuring-bind ,arguments arguments
           ,length))
       (defmethod render-value-of-type (,value (#:name (eql ',name)) arguments)
         (destructuring-bind ,arguments arguments
           ,@body)))))

(defgeneric type-length (type-name arguments))
(defgeneric render-value-of-type (value type-name arguments))

(defmacro define-enum ((name bytes) &body body)
  `(progn
     (defmethod type-length ((type-name (eql ',name)) arguments)
       (declare (ignore arguments))
       ,bytes)
     (defmethod render-value-of-type (value (type-name (eql ',name)) arguments)
       (declare (ignore arguments))
       (let ((bit-alist ',body)
             (integer 0))
         (dolist (flag value)
           (let ((pair (assoc flag bit-alist)))
             (if (null pair)
                 (error "unknown flag for ~s: ~s" ',name flag)
                 (setf integer (logior integer (second pair))))))
         (loop for byte from ,(1- bytes) downto 0
               collect (ldb (byte 8 (* byte 8)) integer))))))
