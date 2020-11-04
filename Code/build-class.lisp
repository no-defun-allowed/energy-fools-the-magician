(in-package :hir-to-jvm)

(defun fields (compiler-state)
  (loop for position below (length (constants compiler-state))
        collect (constant-field-spec position)))

(defun make-invoke-method (compiler-state)
  (make-instance 'jvm-class-format:method
                 :name (jvm-constants:constant-utf8 "call")
                 :descriptor (jvm-constants:constant-utf8
                              (format nil "([Ljava/lang/Object;L~a;)[Ljava/lang/Object;"
                                      (long-class-name "DynamicEnvironmentFrame")))
                 :attributes (list (make-instance 'jvm-class-format:code-attribute
                                                  :attributes #()
                                                  :exception-table #()
                                                  :code (emit-bytes compiler-state)
                                                  :locals-size (length (lexicals compiler-state))
                                                  :stack-size 10))
                 :access-flags '(:public)))

(defun init-method-bytes ()
  (let ((instructions (list (jvm-opcodes:object-local +self+)
                            (jvm-opcodes:invoke-special
                             (jvm-constants:method "java/lang/Object" "<init>" "()V"))
                            (jvm-opcodes:void-return))))
    (reduce #'append instructions
            :key #'jvm-opcodes:render-instruction
            :from-end t)))

(defun make-init-method ()
  (make-instance 'jvm-class-format:method
                 :name (jvm-constants:constant-utf8 "<init>")
                 :descriptor (jvm-constants:constant-utf8 "()V")
                 :attributes (list (make-instance 'jvm-class-format:code-attribute
                                                  :attributes #()
                                                  :exception-table #()
                                                  :code (init-method-bytes)
                                                  :locals-size 1
                                                  :stack-size 10))
                 :access-flags '(:public)))

(defun make-class-file (compiler-state)
  (make-instance 'jvm-class-format:class-file
                 :constant-pool jvm-constants:*constant-pool*
                 :this-class (jvm-constants:class-named (name compiler-state))
                 :super-class (jvm-constants:class-named "java/lang/Object")
                 :interfaces #()
                 :access-flags '(:public)
                 :attributes #()
                 :methods (list (make-invoke-method compiler-state)
                                (make-init-method)
                                (make-rebuild-constants-method compiler-state))
                 :fields (fields compiler-state)
                 :interfaces #()))
