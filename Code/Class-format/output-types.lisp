(in-package :jvm-opcodes)

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
     (defmethod type-length ((type-name (eql ',name))) ,byte)
     (defmethod render-value-of-type (value (type-name (eql ',name)))
       (let ((bit-alist ',body)
             (value 0))
         (dolist (flag value)
           (let ((pair (assoc flag bit-alist)))
             (if (null pair)
                 (error "unknown flag for ~s: ~s" ',name flag)
                 (setf value (logior value (second pair))))))
         (loop for byte from ,bytes downto 0
               collect (ldb (byte 8 (* byte 8)) value))))))
