(in-package :jvm-class-format)

(defclass attribute ()
  ())

(defgeneric attribute-name (attribute))

(defmethod render-value-of-type :around
    (attribute (type (eql 'attribute)) arguments)
  (let ((rest (call-next-method)))
    (list (render-value-of-type (jvm-constants:constant-utf8
                                 (attribute-name attribute))
                                'jvm-constants:constant '())
          (render-value-of-type (io-list-length rest)
                                'jvm-binary-output:int '())
          rest)))

(defmacro define-attribute ((class-name textual-name) &body slots)
  `(progn
     (define-record ,class-name ()
       ,slots
       :superclasses (attribute)
       :output-type attribute)
     (defmethod attribute-name ((attribute ,class-name))
       ,textual-name)))

(define-attribute (code-attribute "Code")
  (maximum-stack   :type short :initarg :stack-size)
  (maximum-locals  :type short :initarg :locals-size)
  (code            :type (pool byte int) :initarg :code)
  (exception-table :type (pool exception-entry) :initarg :exception-table)
  (attributes      :type (pool attribute) :initarg :attributes))
