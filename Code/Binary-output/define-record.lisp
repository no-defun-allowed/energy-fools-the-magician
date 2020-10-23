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

(defmacro define-record (name type-variables &body slots)
  `(progn
     (defclass ,name ()
       ,(mapcar #'process-record-slot slots))
     (defmethod render-value-of-type (value (type-name (eql ',name)) arguments)
       (declare (ignore arguments))
       (append ,@(loop for (slot-name . slot-plist) in slots
                       for type = (getf slot-plist :type)
                       collect `(render-value-of-type (,slot-name value)
                                                      ,@(type-specifier-for-slot-type type-variables
                                                                                      type
                                                                                      'value)))))))
