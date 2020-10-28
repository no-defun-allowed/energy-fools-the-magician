(in-package :jvm-opcodes)

(define-record class-file ()
  ((magic         :type int   :initform #xcafebabe)
   (minor-version :type short :initform 0)
   (major-version :type short :initform 52)
   (access-flags :type access-flags :initarg :access-flags)
   (this-class   :type constant :initarg :this-class)
   (super-class  :type constant :initarg :super-class)
   (interfaces   :type (pool constant) :initarg :interfaces)
   (fields       :type (pool field) :initarg :fields)
   (methods      :type (pool method) :initarg :methods)
   (attributes   :type (pool attributes) :initarg :attributes)
   (constant-pool :type constant-pool :initarg :constant-pool))
  :order (magic minor-version major-version
          constant-pool access-flags
          this-class super-class
          interfaces fields methods attributes))

(defmethod render-value-of-type (sequence (type-name (eql 'pool)) arguments)
  (destructuring-bind (element-type) arguments
    (let ((elements
            (map 'list (lambda (element)
                         (render-value-of-type element element-type '()))
                 sequence)))
      (cons (render-value-of-type (length sequence) 'short '())
            elements))))

(define-enum (access-flags 2)
  (:public     #x0001)
  (:private    #x0002)
  (:protected  #x0004)
  (:static     #x0008)
  (:final      #x0010)
  (:super      #x0020)
  (:volatile   #x0040)
  (:transient  #x0080)
  (:interface  #x0200)
  (:abstract   #x0400)
  (:synthetic  #x1000)
  (:annotation #x2000)
  (:enum       #x4000)
  (:module     #x8000))
