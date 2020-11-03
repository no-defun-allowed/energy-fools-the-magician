(in-package :jvm-binary-output)

(defun process-record-slot (slot)
  `(,(first slot) ,@(alexandria:remove-from-plist (rest slot) :type)
                  :reader ,(first slot)))

(defun type-specifier-for-slot-type (type-variables type value)
  (destructuring-bind (name &rest arguments)
      (alexandria:ensure-list type)
    (flet ((process (part)
             (if (member part type-variables)
                 `(slot-value ,value ',part)
                 `',part)))
      `(,(process name) (list ,@(mapcar #'process arguments))))))

(defmacro define-record* (name slots &key (superclasses '()))
  `(defclass ,name ,superclasses
     ,(mapcar #'process-record-slot slots)))

(defmacro define-record (name type-variables slots
                         &key (order (mapcar #'first slots))
                              (superclasses '())
                              (output-type name))
  (let ((rendered-table (make-hash-table)))
    `(progn
       (define-record* ,name ,slots :superclasses ,superclasses)
       (defmethod render-value-of-type (value (type-name (eql ',output-type)) arguments)
         (declare (ignore arguments))
         (let ,(loop for (slot-name . slot-plist) in slots
                     for type = (getf slot-plist :type)
                     for value = (gensym "RENDERED")
                     do (setf (gethash slot-name rendered-table) value)
                     collect `(,value
                               (render-value-of-type (,slot-name value)
                                                     ,@(type-specifier-for-slot-type type-variables
                                                                                     type
                                                                                     'value))))
           (list ,@(loop for name in order collect (gethash name rendered-table))))))))
