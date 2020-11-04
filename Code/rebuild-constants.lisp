(in-package :hir-to-jvm)

(defun constant-field-reference (class-name position)
  "Produce a field reference for the position'th constant value."
  (jvm-constants:field class-name
                       (format nil "constant~d" position)
                       "Ljava/lang/Object;"))
(defun constant-field-spec (position)
  (make-instance 'jvm-class-format:field
                 :attributes '()
                 :descriptor (jvm-constants:constant-utf8 "Ljava/lang/Object;")
                 :name (jvm-constants:constant-utf8 (format nil "constant~d" position))
                 :access-flags '(:private :static)))

(defun long-class-name (name)
  (concatenate 'string "org/appliedlanguage/energyfoolsthemagician/" name))

(defgeneric rebuild-constant (value field))

(defmethod rebuild-constant ((value integer) field)
  (list
   (jvm-opcodes:object-local +self+)
   (jvm-opcodes:long-constant (jvm-constants:constant-long value))
   (jvm-opcodes:invoke-static
    (jvm-constants:method (long-class-name "Fixnum")
                          "ofValue"
                          (format nil "(J)L~a;" (long-class-name "Fixnum"))))
   (jvm-opcodes:static-set field)))

(defun rebuild-constants-code (compiler-state)
  (let ((instructions
          (append (loop for (value field) across (constants compiler-state)
                        appending (rebuild-constant value field))
                  (list (jvm-opcodes:void-return)))))
    (reduce #'append instructions :key #'jvm-opcodes:render-instruction :from-end t)))

(defun make-rebuild-constants-method (compiler-state)
  (make-instance 'jvm-class-format:method
                 :attributes (list
                              (make-instance 'jvm-class-format:code-attribute
                                             :attributes '()
                                             :exception-table #()
                                             :code (rebuild-constants-code compiler-state)
                                             :locals-size 1
                                             :stack-size 10))
                 :access-flags '(:public)
                 :descriptor (jvm-constants:constant-utf8 "()V")
                 :name (jvm-constants:constant-utf8 "initialize")))
