(in-package :jvm-constants)

(defstruct constant-pool
  (vector (make-array 0 :adjustable t :fill-pointer 0))
  (table  (make-hash-table :test 'equal)))

(defun add-constant-to-pool (constant-pool constant)
  (let ((index (vector-push-extend constant
                                   (constant-pool-vector constant-pool))))
    (setf (gethash constant (constant-pool-table constant-pool)) index)
    index))
(defun find-constant-in-pool (constant-pool constant)
  (multiple-value-bind (index present?)
      (gethash constant (constant-pool-table constant-pool))
    (if present?
        index
        (add-constant-to-pool constant-pool constant))))

(defvar *constant-pool* (make-constant-pool))

(defmacro with-constant-pool (() &body body)
  "Bind *CONSTANT-POOL* to a fresh constant pool while evaluating the forms in BODY."
  `(let ((*constant-pool* (make-constant-pool)))
     ,@body))

(defmethod render-value-of-type
    (c (type-name (eql 'constant)) arguments)
  (declare (ignore arguments))
  (render-value-of-type
   (1+ (find-constant-in-pool *constant-pool* c)) 'short '()))
(defmethod type-length ((type-name (eql 'constant)) arguments)
  (declare (ignore arguments))
  2)

(defmethod render-value-of-type
    (pool (type-name (eql 'constant-pool)) arguments)
  (declare (ignore arguments))
  (loop with vector = (constant-pool-vector pool)
        for position from 0
        until (= position (length vector))
        collect (render-value-of-type
                 (aref vector position) 'constant-info '())
          into values
        finally (return (cons (render-value-of-type (1+ (length vector))
                                                    'short '())
                              values))))

(defun render-constant (c)
  (%render-constant (first c) (rest c)))
(defgeneric %render-constant (name parts))

(defmethod render-value-of-type
    (c (type-name (eql 'constant-info)) arguments)
  (declare (ignore arguments))
  (render-constant c))

(defmacro define-constant-type (name tag &rest arguments)
  (let ((argument-names (mapcar #'first  arguments))
        (type-names     (mapcar #'second arguments)))
    `(progn
       (defun ,name ,argument-names
         (list ',name ,@argument-names))
       (defmethod %render-constant ((name (eql ',name)) parts)
         (destructuring-bind ,argument-names parts
           (cons ,tag
                 (list
                  ,@(loop for argument-name in argument-names
                          for type-specifier in type-names
                          for (type-name . type-arguments)
                            = (alexandria:ensure-list type-specifier)
                          collect `(render-value-of-type
                                    ,argument-name ',type-name ',type-arguments)))))))))

(defmethod render-value-of-type
    (string (type-name (eql 'string)) arguments)
  (declare (ignore arguments))
  (let ((bytes (babel:string-to-octets string :encoding :utf-8)))
    (list (render-value-of-type (length bytes) 'short '())
          bytes)))

(define-constant-type constant-utf8    1 (data string))
(define-constant-type constant-integer 3 (value signed-int))
(define-constant-type constant-float   4 (value float))
(define-constant-type constant-long    5 (value signed-long))
(define-constant-type constant-double  6 (value double))
(define-constant-type constant-class   7 (name constant))
(define-constant-type constant-string 8 (value string)) 
(define-constant-type constant-field   9 (class constant) (name-and-type constant))
(define-constant-type constant-method  10 (class constant) (name-and-type constant))
(define-constant-type constant-interface-method 11 (class constant) (name-and-type constant))
(define-constant-type constant-name-and-type 12 (name constant) (descriptor constant))
(define-constant-type constant-method-handle 15 (kind byte) (index constant))
(define-constant-type constant-method-type   16 (descriptor constant))

(defun class-named (name)
  (constant-class (constant-utf8 name)))
