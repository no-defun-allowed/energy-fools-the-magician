(in-package :jvm-class-format)

(define-record method ()
  ((access-flags :type access-flags :initarg :access-flags)
   (name         :type jvm-constants:constant :initarg :name)
   (descriptor   :type jvm-constants:constant :initarg :descriptor)
   (attributes   :type (pool attribute) :initarg :attributes)))

(defun render-attribute (attribute)
  (let ((content (render-value-of-type attribute
                                       'method-attribute-content '())))
    (list (render-value-of-type (attribute-name attribute))
          (render-value-of-type (io-list-length content)
                                'short '())
          content)))
        
